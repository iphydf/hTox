{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeOperators       #-}
-- | SQLite database: per account its keys, profile, friends, requests and
-- chats, plus the device settings. We load the model at the start, and
-- write back what each event changes.
module HTox.Store
  ( Database
  , Store
  , open
  , opened
  , schemaVersion
  , Newer (..)
  , Locked (..)
  , profiles
  , activeProfile
  , setActive
  , newProfile
  , newLinkedProfile
  , deleteProfile
  , backUp
  , writing
  , written
  , flush
  , restore
  , profileStore
  , identity
  , saveIdentity
  , load
  , older
  , recalled
  , save
  ) where

import           Control.Applicative       ((<|>))
import           Control.Concurrent.MVar   (MVar, newMVar, withMVar)
import           Control.Concurrent.STM    (TVar, atomically, check,
                                            modifyTVar', newTVarIO, readTVar,
                                            readTVarIO)
import           Control.Exception         (Exception (..), IOException,
                                            finally, throwIO, try)
import           Control.Monad             (filterM, forM, forM_, unless, void,
                                            when)
import qualified Crypto.Saltine.Class      as Sodium
import           Crypto.Saltine.Core.Utils (randomByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as Base16
import           Data.Int                  (Int64)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (catMaybes, fromMaybe, isJust,
                                            listToMaybe, mapMaybe)
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Time                 (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX     (posixSecondsToUTCTime,
                                            utcTimeToPOSIXSeconds)
import           Data.Word                 (Word32, Word64)
import           Database.SQLite.Simple    (Only (..), execute, execute_, query,
                                            query_, withTransaction, (:.) (..))
import qualified Database.SQLite.Simple    as SQLite
import           System.Directory          (doesFileExist, removeFile,
                                            renameFile)
import           System.IO                 (IOMode (ReadMode), withBinaryFile)
import qualified Tox.Application.Tox       as Tox
import           Tox.Client                (initialClient)
import           Tox.Client.Changes
import qualified Tox.Client.Home           as Home
import qualified Tox.Client.Linked         as Linked
import           Tox.Client.Types          (Ids (..), MessageId (..))
import qualified Tox.Client.Update         as Client
import qualified Tox.Crypto.Core.KeyPair   as KeyPair
import qualified Tox.Share.Info            as Info
import qualified Tox.Sync.Id               as Sync
import qualified Tox.Sync.Store            as Sync

import           HTox.Model
import           HTox.Private              (private)

data Database = Database
  { dbLock       :: MVar ()
    -- ^ One thread at a time (each online profile saves from its own).
  , dbQueued     :: TVar (Seq (Int64, [Change]))
    -- ^ Saved, by profile, but not yet written.
  , dbConnection :: SQLite.Connection
  }

data Store = Store Database ProfileId

-- | After what was saved is written, so all of it is there.
using :: Database -> (SQLite.Connection -> IO a) -> IO a
using database f = withMVar (dbLock database) $ \() -> writeQueued database >> f (dbConnection database)

-- | In one transaction. Kept queued if it fails, to try again.
writeQueued :: Database -> IO ()
writeQueued database = do
  queued <- readTVarIO (dbQueued database)
  unless (Seq.null queued) $ do
    let db = dbConnection database
    withTransaction db $ forM_ queued $ \(i, todo) -> mapM_ (apply db i) todo
    atomically $ modifyTVar' (dbQueued database) (Seq.drop (Seq.length queued))

-- | Writes what is saved as it comes, so no event waits for the disk, each
-- write timed by the second. Says why a write failed, and tries again once
-- more is saved.
writing :: (Text -> IO ()) -> (IO () -> IO ()) -> Database -> IO ()
writing complain timed database = go 0
  where
    go stuck = do
      atomically $ readTVar (dbQueued database) >>= check . (> stuck) . Seq.length
      try (timed (flush database)) >>= \case
        Right ()                      -> go 0
        Left (err :: SQLite.SQLError) -> do
          complain ("Not saved yet: " <> Text.pack (displayException err))
          go . Seq.length =<< readTVarIO (dbQueued database)

-- | Once all that was saved is written. Tox is told nothing before what it
-- would do is saved.
written :: Database -> IO ()
written database = atomically $ readTVar (dbQueued database) >>= check . Seq.null

-- | Writes what was saved now, as the app goes away.
flush :: Database -> IO ()
flush database = using database (const (pure ()))

newtype Newer = Newer Int
  deriving (Show)

instance Exception Newer where
  displayException (Newer version) =
    "The chats were saved by a newer hTox (version " <> show version <> "). Install that one again."

data Locked = Locked
  deriving (Show)

instance Exception Locked where
  displayException Locked =
    "The phone's keystore gave no key for the chats, so they can't be opened."

-- | Encrypted with the key if there is one. An old plain database gets
-- encrypted first.
open :: Maybe BS.ByteString -> FilePath -> IO Database
open Nothing path = SQLite.open path >>= quickCommits >>= opened
open (Just key) path = do
  plain <- isPlain path
  when plain $ encrypt key path
  db <- SQLite.open path
  unlock db key
  quickCommits db >>= opened

-- | Commits append to a log without waiting for the disk, as we save after
-- every event that changes something. A crash loses nothing, a power cut may
-- lose the latest, but never the database.
quickCommits :: SQLite.Connection -> IO SQLite.Connection
quickCommits db = do
  void (query_ db "PRAGMA journal_mode = WAL" :: IO [Only Text])
  execute_ db "PRAGMA synchronous = NORMAL"
  pure db

isPlain :: FilePath -> IO Bool
isPlain path = do
  exists <- doesFileExist path
  if exists
    then (== "SQLite format 3\0") <$> withBinaryFile path ReadMode (`BS.hGet` 16)
    else pure False

-- | Sets the key, then reads something, which fails if the key is wrong.
unlock :: SQLite.Connection -> BS.ByteString -> IO ()
unlock db key = do
  execute_ db (SQLite.Query ("PRAGMA key = \"" <> rawKey key <> "\""))
  void (query_ db "SELECT count(*) FROM sqlite_master" :: IO [Only Int])

-- | Raw key for SQLCipher (it's random already, so no key derivation).
rawKey :: BS.ByteString -> Text
rawKey key = "x'" <> Text.decodeUtf8 (Base16.encode key) <> "'"

-- | Encrypts into a new file, which then replaces the plain one.
encrypt :: BS.ByteString -> FilePath -> IO ()
encrypt key path = do
  let encrypted = path <> ".encrypting"
  void (try (removeFile encrypted) :: IO (Either IOException ()))
  plain <- SQLite.open path
  [Only version] <- query_ plain "PRAGMA user_version" :: IO [Only Int]
  execute plain "ATTACH DATABASE ? AS encrypted KEY ?" (encrypted, rawKey key)
  void (query_ plain "SELECT sqlcipher_export('encrypted')" :: IO [Only (Maybe Int)])
  execute_ plain (SQLite.Query ("PRAGMA encrypted.user_version = " <> Text.pack (show version)))
  execute_ plain "DETACH DATABASE encrypted"
  SQLite.close plain
  renameFile encrypted path

-- | What 'opened' takes every database to.
schemaVersion :: Int
schemaVersion = length migrations

opened :: SQLite.Connection -> IO Database
opened db = do
  [Only version] <- query_ db "PRAGMA user_version"
  when (version > length migrations) $ throwIO (Newer version)
  forM_ (zip [1 ..] (drop version migrations)) $ \(done, steps) -> withTransaction db $ do
    mapM_ (execute_ db) steps
    execute_ db (SQLite.Query ("PRAGMA user_version = " <> Text.pack (show (version + done :: Int))))
  Database <$> newMVar () <*> newTVarIO Seq.empty <*> pure db

-- | Each step takes the database to the next version, the first from
-- nothing.
migrations :: [[SQLite.Query]]
migrations =
  [ [ -- Keys (once Tox made them) and account settings.
      "CREATE TABLE profiles\
      \ (id INTEGER PRIMARY KEY, secret_key BLOB, nospam INTEGER, nickname TEXT NOT NULL, status TEXT NOT NULL,\
      \ away INTEGER NOT NULL DEFAULT 0, avatar BLOB, auto_accept INTEGER NOT NULL, formats_off TEXT NOT NULL DEFAULT '',\
      \ home TEXT, pairing TEXT, linked_at INTEGER)"
    , "CREATE TABLE contacts\
      \ (profile INTEGER NOT NULL REFERENCES profiles (id), key TEXT NOT NULL, name TEXT, unread INTEGER NOT NULL,\
      \ request_address TEXT, request_message TEXT, wanted INTEGER NOT NULL DEFAULT 1,\
      \ status_message TEXT NOT NULL DEFAULT '', muted INTEGER NOT NULL DEFAULT 0, muted_until INTEGER,\
      \ avatar BLOB, send_pictures TEXT, PRIMARY KEY (profile, key))"
    , "CREATE TABLE messages\
      \ (profile INTEGER NOT NULL, contact TEXT NOT NULL, place INTEGER NOT NULL,\
      \ mine INTEGER NOT NULL, text TEXT NOT NULL, time INTEGER NOT NULL, delivery TEXT NOT NULL,\
      \ file_size INTEGER, file_path TEXT, file_done INTEGER, file_state TEXT,\
      \ call_video INTEGER, call_duration INTEGER, call_missed INTEGER, parts INTEGER NOT NULL DEFAULT 0,\
      \ file_id BLOB, raw BLOB, kind INTEGER NOT NULL DEFAULT 0, delivered_time INTEGER, id BLOB NOT NULL,\
      \ PRIMARY KEY (profile, contact, place), FOREIGN KEY (profile, contact) REFERENCES contacts (profile, key))"
    , -- ID that stays when the message changes.
      "CREATE UNIQUE INDEX messages_id ON messages (profile, id)"
    , "CREATE TABLE requests\
      \ (profile INTEGER NOT NULL REFERENCES profiles (id), key TEXT NOT NULL, message TEXT NOT NULL,\
      \ PRIMARY KEY (profile, key))"
    , "CREATE TABLE declined\
      \ (profile INTEGER NOT NULL REFERENCES profiles (id), key TEXT NOT NULL, message TEXT NOT NULL,\
      \ PRIMARY KEY (profile, key))"
    , "CREATE TABLE drafts\
      \ (profile INTEGER NOT NULL REFERENCES profiles (id), key TEXT NOT NULL, message TEXT NOT NULL,\
      \ PRIMARY KEY (profile, key))"
    , -- Our and friends' avatars, by SHA-256.
      "CREATE TABLE avatars\
      \ (profile INTEGER NOT NULL REFERENCES profiles (id), hash BLOB NOT NULL, png BLOB NOT NULL,\
      \ PRIMARY KEY (profile, hash))"
    , -- Also the active account.
      "CREATE TABLE device_settings\
      \ (id INTEGER PRIMARY KEY CHECK (id = 0), udp INTEGER NOT NULL, ipv6 INTEGER NOT NULL, lan INTEGER NOT NULL,\
      \ stay_online INTEGER NOT NULL, active_profile INTEGER REFERENCES profiles (id),\
      \ proxy_type TEXT, proxy_host TEXT, proxy_port INTEGER)"
    , "INSERT INTO device_settings (id, udp, ipv6, lan, stay_online) VALUES (0, 1, 1, 1, 1)"
    , -- The saved group has our keys in it.
      "CREATE TABLE groups\
      \ (profile INTEGER NOT NULL REFERENCES profiles (id), chat_id TEXT NOT NULL, name TEXT NOT NULL,\
      \ phase TEXT NOT NULL, unread INTEGER NOT NULL, saved BLOB, muted INTEGER NOT NULL DEFAULT 0, muted_until INTEGER,\
      \ draft TEXT, mentions INTEGER NOT NULL DEFAULT 1, topic TEXT NOT NULL DEFAULT '',\
      \ kind TEXT NOT NULL DEFAULT 'group', PRIMARY KEY (profile, chat_id))"
    , -- Messages with the sender's name at the time, joins and leaves.
      "CREATE TABLE group_entries\
      \ (profile INTEGER NOT NULL, chat_id TEXT NOT NULL, place INTEGER NOT NULL, time INTEGER NOT NULL,\
      \ what TEXT NOT NULL, member_key TEXT, member_name TEXT, kind INTEGER NOT NULL DEFAULT 0, text TEXT,\
      \ delivery TEXT, other_key TEXT, other_name TEXT, id BLOB NOT NULL, PRIMARY KEY (profile, chat_id, place),\
      \ FOREIGN KEY (profile, chat_id) REFERENCES groups (profile, chat_id))"
    , "CREATE UNIQUE INDEX group_entries_id ON group_entries (profile, id)"
    , -- Member key of the relay bot in each group.
      "CREATE TABLE relay_bots\
      \ (profile INTEGER NOT NULL, chat_id TEXT NOT NULL, bot TEXT NOT NULL, PRIMARY KEY (profile, chat_id),\
      \ FOREIGN KEY (profile, chat_id) REFERENCES groups (profile, chat_id))"
    , -- A file (our copy of it) or a location.
      "CREATE TABLE staged\
      \ (profile INTEGER NOT NULL REFERENCES profiles (id), contact TEXT NOT NULL, place INTEGER NOT NULL,\
      \ path TEXT, name TEXT, size INTEGER, file_id BLOB, latitude REAL, longitude REAL,\
      \ PRIMARY KEY (profile, contact, place))"
    , "CREATE TABLE invites\
      \ (profile INTEGER NOT NULL REFERENCES profiles (id), friend TEXT NOT NULL, chat_id TEXT NOT NULL,\
      \ name TEXT NOT NULL, data BLOB NOT NULL, answered INTEGER NOT NULL DEFAULT 0, password TEXT,\
      \ kind TEXT NOT NULL DEFAULT 'group',\
      \ PRIMARY KEY (profile, friend, chat_id))"
    , -- Linked accounts' events, as the home and other devices wrote them.
      -- The chats are derived from these.
      "CREATE TABLE sync_events\
      \ (profile INTEGER NOT NULL REFERENCES profiles (id), id BLOB NOT NULL, rank INTEGER NOT NULL,\
      \ bytes BLOB NOT NULL, PRIMARY KEY (profile, id))"
    ]
  , [ -- Where new message IDs come from.
      "ALTER TABLE profiles ADD COLUMN id_seed BLOB"
    , "UPDATE profiles SET id_seed = randomblob(32)"
    , "ALTER TABLE profiles ADD COLUMN ids_used INTEGER NOT NULL DEFAULT 0"
    ]
  , [ -- Tox message IDs for group history sync, and files shared in groups.
      "ALTER TABLE group_entries ADD COLUMN tox_id INTEGER"
    , "ALTER TABLE group_entries ADD COLUMN file_id BLOB"
    , "ALTER TABLE group_entries ADD COLUMN file_info BLOB"
    , "ALTER TABLE group_entries ADD COLUMN file_size INTEGER"
    , "ALTER TABLE group_entries ADD COLUMN file_path TEXT"
    , "ALTER TABLE group_entries ADD COLUMN file_done INTEGER"
    , "ALTER TABLE group_entries ADD COLUMN file_state TEXT"
    ]
  , [ -- Settings as rows by name, so a new one needs no new column: one
      -- without a row has its default.
      "CREATE TABLE account_settings\
      \ (profile INTEGER NOT NULL REFERENCES profiles (id), name TEXT NOT NULL, value NOT NULL,\
      \ PRIMARY KEY (profile, name))"
    , "INSERT INTO account_settings SELECT id, 'auto_accept', auto_accept FROM profiles"
    , "ALTER TABLE profiles DROP COLUMN auto_accept"
    , "ALTER TABLE profiles DROP COLUMN formats_off"
    -- What's left is the active profile.
    , "ALTER TABLE device_settings RENAME TO device"
    , "CREATE TABLE device_settings (name TEXT PRIMARY KEY, value NOT NULL)"
    ]
    ++ [ "INSERT INTO device_settings SELECT '" <> column <> "', " <> column <> " FROM device WHERE "
         <> column <> " IS NOT NULL"
       | column <- deviceColumns ]
    ++ [ "ALTER TABLE device DROP COLUMN " <> column | column <- deviceColumns ]
  , [ -- The picture format sent in each group.
      "ALTER TABLE groups ADD COLUMN pictures TEXT"
    ]
  ]
  where
    deviceColumns = ["udp", "ipv6", "lan", "stay_online", "proxy_type", "proxy_host", "proxy_port"]

-- | Oldest first.
profiles :: Database -> IO [Account]
profiles database = using database accounts

accounts :: SQLite.Connection -> IO [Account]
accounts db = map account <$> query_ db "SELECT id, nickname, secret_key FROM profiles ORDER BY id"
  where
    account (i, name, secret) = Account (ProfileId i) name $ case Sodium.decode =<< secret of
      Just sk -> Text.toUpper (Text.decodeUtf8 (Base16.encode (Sodium.encode (KeyPair.publicKey (KeyPair.fromSecretKey sk)))))
      Nothing -> ""

-- | Last one chosen, or the first, or a new one.
activeProfile :: Database -> IO ProfileId
activeProfile database = do
  chosen <- using database $ \db -> query_ db "SELECT active_profile FROM device WHERE active_profile IN (SELECT id FROM profiles)"
  case chosen of
    [Only (Just i)] -> pure (ProfileId i)
    _ -> profiles database >>= \case
      first : _ -> accountId first <$ setActive database (accountId first)
      [] -> newProfile database Nothing (clientProfile initialClient) >>= \new -> new <$ setActive database new

setActive :: Database -> ProfileId -> IO ()
setActive database (ProfileId i) = using database $ \db -> execute db "UPDATE device SET active_profile = ?" (Only i)

-- | With its keys if it came from elsewhere.
newProfile :: Database -> Maybe Identity -> Profile -> IO ProfileId
newProfile database known profile = using database $ \db -> do
  execute db "INSERT INTO profiles (secret_key, nospam, nickname, status, id_seed)\
             \ VALUES (?, ?, ?, ?, randomblob(32))"
    (identitySecretKey <$> known, identityNospam <$> known, profileNickname profile, profileStatus profile)
  ProfileId <$> SQLite.lastInsertRowId db

-- | Linked to an account on another device (its home), by the home's Tox
-- address and the pairing friend request. Makes our device key here (the
-- account's events name it).
newLinkedProfile :: Database -> Text -> Text -> UTCTime -> IO ProfileId
newLinkedProfile database home pairing now = using database $ \db -> do
  pair <- KeyPair.newKeyPair
  nospam <- BS.foldl' (\n x -> n * 256 + fromIntegral x) (0 :: Word32) <$> randomByteString 4
  execute db "INSERT INTO profiles (secret_key, nospam, nickname, status, home, pairing, linked_at, id_seed)\
             \ VALUES (?, ?, ?, ?, ?, ?, ?, randomblob(32))"
    ( (Sodium.encode (KeyPair.secretKey pair), nospam, "" :: Text, "" :: Text)
    :. (home, pairing, toMillis now) )
  ProfileId <$> SQLite.lastInsertRowId db

-- | With its chats and their files.
deleteProfile :: Database -> ProfileId -> IO ()
deleteProfile database (ProfileId i) = using database $ \db -> withTransaction db $ do
  paths <- SQLite.query db "SELECT file_path FROM messages WHERE profile = ? AND file_path IS NOT NULL" (Only i)
  forM_ paths $ \(Only path) -> void (try (removeFile path) :: IO (Either IOException ()))
  forM_ ("staged" : owned) $ \table ->
    execute db (SQLite.Query ("DELETE FROM " <> table <> " WHERE profile = ?")) (Only i)
  execute db "UPDATE device SET active_profile = NULL WHERE active_profile = ?" (Only i)
  execute db "DELETE FROM profiles WHERE id = ?" (Only i)

-- | Tables with rows of each profile, besides staged files (a backup has no
-- files).
owned :: [Text]
owned =
  [ "contacts", "messages", "requests", "declined", "drafts", "avatars", "groups", "group_entries", "relay_bots"
  , "invites", "sync_events", "account_settings" ]

-- | Copies the profiles into a new plain database, to restore here or on
-- another phone. Device settings stay here.
backUp :: Database -> [ProfileId] -> FilePath -> IO ()
backUp database pids path = do
  private path
  SQLite.withConnection path $ \backup -> do
    void (opened backup)
    execute_ backup "DELETE FROM device_settings"
    execute_ backup "UPDATE device SET active_profile = NULL"
  attached database path $ \db -> forM_ pids $ \(ProfileId i) -> do
    execute db "INSERT INTO backup.profiles SELECT * FROM main.profiles WHERE id = ?" (Only i)
    forM_ owned $ \table ->
      execute db (SQLite.Query ("INSERT INTO backup." <> table <> " SELECT * FROM main." <> table <> " WHERE profile = ?"))
        (Only i)

-- | Adds the backup's profiles we don't have yet, with new IDs, and returns
-- them. Their files the phone doesn't have show as not received or sent.
restore :: Database -> FilePath -> IO [ProfileId]
restore database path = do
  -- Brings an older backup up to date, and refuses a newer one.
  SQLite.withConnection path (void . opened)
  attached database path $ \db -> do
    here <- query_ db "SELECT secret_key FROM main.profiles WHERE secret_key IS NOT NULL" :: IO [Only BS.ByteString]
    theirs <- query_ db "SELECT id, secret_key FROM backup.profiles ORDER BY id" :: IO [(Int64, Maybe BS.ByteString)]
    -- Both have the same tables now, but each row gets a new profile ID.
    let columnsBut :: Text -> Text -> IO Text
        columnsBut column table = Text.intercalate ", " . filter (/= column) . map fromOnly
          <$> query db "SELECT name FROM pragma_table_info(?)" (Only table)
    fmap catMaybes . forM theirs $ \(old, key) ->
      if maybe False ((`elem` here) . Only) key then pure Nothing else do
        names <- columnsBut "id" "profiles"
        execute db (SQLite.Query ("INSERT INTO main.profiles (" <> names <> ") SELECT " <> names
                                  <> " FROM backup.profiles WHERE id = ?")) (Only old)
        new <- SQLite.lastInsertRowId db
        forM_ owned $ \table -> do
          names' <- columnsBut "profile" table
          execute db (SQLite.Query ("INSERT INTO main." <> table <> " (profile, " <> names' <> ") SELECT ?, " <> names'
                                    <> " FROM backup." <> table <> " WHERE profile = ?")) (new, old)
        files <- query db "SELECT contact, place, file_path FROM messages WHERE profile = ? AND file_path IS NOT NULL" (Only new)
        forM_ files $ \(contact, place, file) -> do
          kept <- doesFileExist file
          unless kept $ execute db "UPDATE messages SET file_state = 'cancelled' WHERE profile = ? AND contact = ? AND place = ?"
            (new, contact :: Text, place :: Int)
        pure (Just (ProfileId new))

-- | In one transaction, with the plain database at the path attached as
-- "backup".
attached :: Database -> FilePath -> (SQLite.Connection -> IO a) -> IO a
attached database path f = using database $ \db -> do
  -- An empty key keeps it plain, even with an encrypted main database.
  execute db "ATTACH DATABASE ? AS backup KEY ''" (Only path)
  withTransaction db (f db) `finally` execute_ db "DETACH DATABASE backup"

profileStore :: Database -> ProfileId -> Store
profileStore = Store

identity :: Store -> IO (Maybe Identity)
identity (Store database (ProfileId i)) = using database $ \db -> do
  found <- SQLite.query db "SELECT secret_key, nospam FROM profiles WHERE id = ?" (Only i)
  pure $ case found of
    [(Just key, Just nospam)] -> Just (Identity key nospam)
    _                         -> Nothing

saveIdentity :: Store -> Identity -> IO ()
saveIdentity (Store database (ProfileId i)) (Identity key nospam) = using database $ \db ->
  execute db "UPDATE profiles SET secret_key = ?, nospam = ? WHERE id = ?" (key, nospam, i)

-- | History a member sent, without what we have already and with the names
-- the group's chat had for its senders, as the chat in memory may not reach
-- back as far.
recalled :: Store -> Client.Event -> IO Client.Event
recalled (Store database (ProfileId i)) = \case
  Client.GroupRecalled gid@(GroupId c) now n names entries -> using database $ \db -> do
    fresh <- filterM (\(_, who, mid, _) -> null <$> (SQLite.query db
      "SELECT 1 FROM group_entries WHERE profile = ? AND chat_id = ? AND member_key IS ? AND tox_id = ? LIMIT 1"
      (i, c, who, fromIntegral mid :: Int64) :: IO [Only Int])) entries
    stored <- forM (Set.toList (Set.fromList [key | (_, Just key, _, _) <- fresh])) $ \key -> fmap ((,) key . fromOnly) . listToMaybe <$> SQLite.query db
      "SELECT member_name FROM group_entries WHERE profile = ? AND chat_id = ? AND member_key = ? AND member_name != ''\
      \ ORDER BY place DESC LIMIT 1" (i, c, key)
    pure (Client.GroupRecalled gid now n (names <> Map.fromList (catMaybes stored)) fresh)
  event -> pure event

-- | The saved model, or the initial one if nothing was saved.
load :: Store -> IO Model
load (Store database pid@(ProfileId i)) = using database $ \db -> do
  let ofProfile q = SQLite.query db q (Only i)
  rows <- ofProfile "SELECT nickname, status, away, avatar FROM profiles WHERE id = ?"
    :: IO [(Text, Text, Int, Maybe BS.ByteString)]
  accountValues <- Map.fromList <$> ofProfile "SELECT name, value FROM account_settings WHERE profile = ?"
  ids <- ofProfile "SELECT id_seed, ids_used FROM profiles WHERE id = ?" :: IO [(Maybe BS.ByteString, Word64)]
  avatars <- ofProfile "SELECT hash, png FROM avatars WHERE profile = ?" :: IO [(BS.ByteString, BS.ByteString)]
  contacts <- ofProfile
    "SELECT key, name, unread, request_address, request_message, status_message, wanted, muted, muted_until,\
    \ avatar, send_pictures FROM contacts WHERE profile = ?"
    :: IO [(Text, Maybe Text, Int, Maybe Text, Maybe Text) :. (Text, Bool, Bool, Maybe Integer, Maybe BS.ByteString) :. Only (Maybe Text)]
  messages <- SQLite.query db (messageWindow <> "SELECT " <> messageColumns <> " FROM messages JOIN starts ON contact = chat\
    \ WHERE profile = ? AND place >= start ORDER BY contact, place") (chatPage, i, i)
  earlierMessages <- SQLite.query db (messageWindow <> "SELECT chat FROM starts WHERE lowest < start") (chatPage, i)
  requests <- ofProfile "SELECT key, message FROM requests WHERE profile = ?" :: IO [(Text, Text)]
  declined <- ofProfile "SELECT key, message FROM declined WHERE profile = ?" :: IO [(Text, Text)]
  drafts <- ofProfile "SELECT key, message FROM drafts WHERE profile = ?" :: IO [(Text, Text)]
  groups <- ofProfile
    "SELECT chat_id, name, phase, unread, saved, muted, muted_until, draft, mentions, topic, kind, pictures\
    \ FROM groups WHERE profile = ?"
    :: IO [(Text, Text, Text, Int, Maybe BS.ByteString) :. (Bool, Maybe Integer, Maybe Text, Bool, Text) :. (Text, Maybe Text)]
  entries <- SQLite.query db (entryWindow <> "SELECT " <> entryColumns <> " FROM group_entries JOIN starts ON chat_id = chat\
    \ WHERE profile = ? AND place >= start ORDER BY chat_id, place") (chatPage, i, i)
  earlierEntries <- SQLite.query db (entryWindow <> "SELECT chat FROM starts WHERE lowest < start") (chatPage, i)
  bots <- ofProfile "SELECT chat_id, bot FROM relay_bots WHERE profile = ?" :: IO [(Text, Text)]
  invites <- ofProfile "SELECT friend, chat_id, name, data, answered, password, kind FROM invites WHERE profile = ?"
    :: IO [(Text, Text, Text, BS.ByteString, Bool, Maybe Text, Text)]
  staged <- ofProfile
    "SELECT contact, path, name, size, file_id, latitude, longitude FROM staged WHERE profile = ? ORDER BY contact, place"
    :: IO [(Text, Maybe FilePath, Maybe Text, Maybe Word64, Maybe BS.ByteString, Maybe Double, Maybe Double)]
  others <- accounts db
  linking <- ofProfile "SELECT home, pairing, linked_at, secret_key FROM profiles WHERE id = ?"
    :: IO [(Maybe Text, Maybe Text, Maybe Integer, Maybe BS.ByteString)]
  events <- ofProfile "SELECT bytes FROM sync_events WHERE profile = ? ORDER BY rank" :: IO [Only BS.ByteString]
  deviceValues <- Map.fromList <$> query_ db "SELECT name, value FROM device_settings"
  let chats = Map.fromListWith (flip (++)) [(cid, [m]) | (cid, m) <- map messageOf messages]
      earlier = Set.fromList . map fromOnly
      kept =
        [ ( wanted
          , ( cid
            , Contact name False (Map.findWithDefault [] cid chats) unread ((,) <$> address <*> request)
                status Tox.Online (if mute then Just (maybe MutedAlways (MutedUntil . fromMillis) until') else Nothing) avatar
                pictures (Set.member key (earlier earlierMessages)) ) )
        | (key, name, unread, address, request) :. (status, wanted, mute, until', avatar) :. Only pictures <- contacts
        , let cid = ContactId key ]
      said = Map.fromListWith (flip (++)) [(gid, [e]) | Just (gid, e) <- map entryOf entries]
      profile = case rows of
        (nickname, status, away, _) : _ -> (clientProfile initialClient)
          { profileNickname = nickname, profileStatus = status, profileAway = awayOf away }
        [] -> clientProfile initialClient
      client = initialClient
        { clientProfile = profile
        , clientProfileId = pid
        , clientAvatar = case rows of
            (_, _, _, avatar) : _ -> avatar
            []                    -> Nothing
        , clientAvatars = Map.fromList avatars
        , clientIds = case ids of
            (seed, used) : _ -> Ids (fromMaybe BS.empty seed) used
            []               -> clientIds initialClient
        , clientContacts = Map.fromList [contact | (True, contact) <- kept]
        , clientFormer = Map.fromList [contact | (False, contact) <- kept]
        , clientRequests = Map.fromList [(ContactId key, message) | (key, message) <- requests]
        , clientDeclined = Map.fromList [(ContactId key, message) | (key, message) <- declined]
        , clientDrafts = Map.fromList [(ContactId key, draft) | (key, draft) <- drafts]
        , clientStaged = Map.fromListWith (flip (++))
            [ (ContactId key, [item])
            | (key, path, name, size, fileId, latitude, longitude) <- staged
            , Just item <- [ (StagedFile <$> path <*> name <*> size <*> fileId)
                               <|> (StagedPlace <$> latitude <*> longitude) ] ]
        , clientGroups = Map.fromList
            [ ( gid
              , (newGroup name (kindOf kind) (phaseOf phase))
                  { groupEntries = Map.findWithDefault [] gid said, groupUnread = unread, groupSaved = saved
                  , groupMute = if mute then Just (maybe MutedAlways (MutedUntil . fromMillis) until') else Nothing
                  , groupMentions = mentioned, groupPictures = pictures, groupTopic = topic, groupRelayBot = lookup chat bots
                  , groupEarlier = Set.member chat (earlier earlierEntries) } )
            | (chat, name, phase, unread, saved) :. (mute, until', _, mentioned, topic) :. (kind, pictures) <- groups
            , let gid = GroupId chat ]
        , clientGroupDrafts = Map.fromList [(GroupId chat, d) | (chat, _, _, _, _) :. (_, _, Just d, _, _) :. _ <- groups]
        , clientInvites = Map.fromList
            [ ( (ContactId friend, GroupId chat)
              , Invite (kindOf kind) name bytes (if answered then Just (fromMaybe "" password) else Nothing) )
            | (friend, chat, name, bytes, answered, password, kind) <- invites ]
        , clientAccount = accountSettings accountValues
        , clientDevice = deviceSettings deviceValues
        , clientLinked = case linking of
            [(Just home, pairing, Just since, Just secret)] | Just sk <- Sodium.decode secret ->
              Just (Linked.newLinked home pairing (fromMillis since) (publicOf sk) held)
            _ -> Nothing
        -- Our own account with events has devices.
        , clientHome = case linking of
            [(Nothing, _, _, Just secret)] | not (null events), Just sk <- Sodium.decode secret ->
              Just (Home.newHome (publicOf sk) held)
            _ -> Nothing
        }
      publicOf sk = Sync.PublicKey (Sodium.encode (KeyPair.publicKey (KeyPair.fromSecretKey sk)))
      held = foldl (\store bytes -> either (const store) snd (Sync.insert bytes store)) Sync.emptyStore [b | Only b <- events]
  pure initial
    { modelClient = client
    , modelProfiles = others
    , modelNickname = profileNickname profile
    , modelStatus = profileStatus profile
    , modelRequest = requestMessage profile
    }

-- | The page before a message in its chat.
older :: Store -> Client.Chat -> MessageId -> IO Client.Event
older (Store database (ProfileId i)) chat (MessageId before) = using database $ \db -> case chat of
  Client.FriendChat cid@(ContactId key) ->
    Client.EarlierMessages cid . reverse . map (snd . messageOf) <$> SQLite.query db
      ("SELECT " <> messageColumns <> " FROM messages WHERE profile = ? AND contact = ? AND place <\
       \ (SELECT place FROM messages WHERE profile = ? AND id = ?) ORDER BY place DESC LIMIT ?") (i, key, i, before, chatPage)
  Client.GroupChat gid@(GroupId c) ->
    Client.EarlierEntries gid . reverse . mapMaybe (fmap snd . entryOf) <$> SQLite.query db
      ("SELECT " <> entryColumns <> " FROM group_entries WHERE profile = ? AND chat_id = ? AND place <\
       \ (SELECT place FROM group_entries WHERE profile = ? AND id = ?) ORDER BY place DESC LIMIT ?") (i, c, i, before, chatPage)

-- | Where each chat starts in memory: at its oldest message still in use
-- ('inUse'), or its newest page if that goes back further. Also its
-- lowest place, so we know if there are earlier ones.
messageWindow :: SQLite.Query
messageWindow =
  "WITH starts (chat, start, lowest) AS (SELECT contact, MIN(MAX(place) - ? + 1, COALESCE(MIN(CASE WHEN\
  \ (mine AND file_size IS NULL AND call_video IS NULL AND delivery <> 'delivered')\
  \ OR file_state IN ('waiting', 'moving', 'interrupted') THEN place END), MAX(place))), MIN(place)\
  \ FROM messages WHERE profile = ? GROUP BY contact) "

-- | As 'messageWindow', for groups ('entryInUse').
entryWindow :: SQLite.Query
entryWindow =
  "WITH starts (chat, start, lowest) AS (SELECT chat_id, MIN(MAX(place) - ? + 1,\
  \ COALESCE(MIN(CASE WHEN delivery = 'pending' THEN place END), MAX(place))), MIN(place)\
  \ FROM group_entries WHERE profile = ? GROUP BY chat_id) "

type MessageRow =
  (Text, Bool, Text, Integer, Text) :. (Maybe Word64, Maybe FilePath, Maybe Word64, Maybe Text)
  :. (Maybe Bool, Maybe Integer, Maybe Bool, Int, Maybe BS.ByteString) :. (Int, Maybe BS.ByteString, Maybe Integer, BS.ByteString)

messageColumns :: SQLite.Query
messageColumns =
  "contact, mine, text, time, delivery, file_size, file_path, file_done, file_state,\
  \ call_video, call_duration, call_missed, parts, file_id, kind, raw, delivered_time, id"

-- | Transfers a restart cut off are interrupted.
messageOf :: MessageRow -> (ContactId, Message)
messageOf ((key, mine, text, time, delivery) :. (size, path, done, state) :. (video, talked, missed, confirmed, fileId)
          :. (kind, raw, delivered, mid)) =
  ( ContactId key
  , interrupted (Message mine text (fromMillis time) (delivery' delivery) file call confirmed
                   (fromMillis <$> delivered) (if kind == 1 then Tox.MessageAction else Tox.MessageNormal) raw (MessageId mid)) )
  where
    file = Attachment <$> size <*> path <*> done <*> (fileState' <$> state) <*> Just fileId
    call = CallRecord <$> video <*> Just (fromMillis' <$> talked) <*> missed

type EntryRow = (Text, Integer, Text, Maybe Text, Maybe Text) :. (Int, Maybe Text, Maybe Text, Maybe Text, Maybe Text) :. Only BS.ByteString
  :. (Maybe Int64, Maybe BS.ByteString, Maybe BS.ByteString, Maybe Word64) :. (Maybe FilePath, Maybe Word64, Maybe Text)

entryColumns :: SQLite.Query
entryColumns = "chat_id, time, what, member_key, member_name, kind, text, delivery, other_key, other_name, id,\
  \ tox_id, file_id, file_info, file_size, file_path, file_done, file_state"

-- | Nothing for what this version doesn't know.
entryOf :: EntryRow -> Maybe (GroupId, GroupEntry)
entryOf ((chat, time, what, key, name) :. (kind, text, delivery, otherKey, otherName) :. Only eid
         :. (messageNumber, fileId, fileInfo, size) :. (path, done, state)) =
  (\happening -> (GroupId chat, GroupEntry (fromMillis time) happening (MessageId eid))) <$> case what of
    "said"         -> Just (Said member kind' (fromMaybe "" text) (maybe Delivered delivery' delivery) toxId')
    "shared file"  -> (\fid -> SharedFile member (GroupFile fid (Info.decode =<< fileInfo) file 0) toxId') <$> fileId
    "private from" -> (\m -> PrivateFrom m kind' (fromMaybe "" text)) <$> member
    "private to"   -> (\m -> PrivateTo m kind' (fromMaybe "" text) (maybe Delivered delivery' delivery)) <$> member
    "moderated"    -> Moderated member other <$> (moderationOf =<< text)
    "arrived"      -> Arrived <$> member
    "departed"     -> Departed <$> member
    "topic"        -> Just (TopicSet member (fromMaybe "" text))
    "noted"        -> Noted <$> text
    _              -> Nothing
  where
    member = Member <$> key <*> name
    other = Member <$> otherKey <*> otherName
    kind' = if kind == 1 then Tox.MessageAction else Tox.MessageNormal
    toxId' = fromIntegral <$> messageNumber
    -- A download a restart cut off is interrupted.
    file = (\s p d st -> Attachment s p d (cutOff (fileState' st)) fileId) <$> size <*> path <*> done <*> state
    cutOff st = if st == Moving then Interrupted else st

-- | Queued, for 'writing' to write, or whatever uses the database next.
save :: Store -> Client -> Client -> IO ()
save (Store database (ProfileId i)) old new =
  unless (null todo) . atomically $ modifyTVar' (dbQueued database) (Seq.|> (i, todo))
  where todo = changes old new

apply :: SQLite.Connection -> Int64 -> Change -> IO ()
apply db i = \case
  ProfileChanged profile avatar ->
    execute db "UPDATE profiles SET nickname = ?, status = ?, away = ?, avatar = ? WHERE id = ?"
      (profileNickname profile, profileStatus profile, fromEnum (profileAway profile), avatar, i)
  IdsUsed used -> execute db "UPDATE profiles SET ids_used = ? WHERE id = ?" (used, i)
  AccountChanged account -> forM_ (accountRows account) $ \(name, value) ->
    execute db "INSERT OR REPLACE INTO account_settings (profile, name, value) VALUES (?, ?, ?)" (i, name, value)
  DeviceChanged device -> forM_ (deviceRows device) $ \case
    (name, Just value) -> execute db "INSERT OR REPLACE INTO device_settings (name, value) VALUES (?, ?)" (name, value)
    (name, Nothing)    -> execute db "DELETE FROM device_settings WHERE name = ?" (Only name)
  AvatarAdded hash png -> execute db "INSERT OR REPLACE INTO avatars (profile, hash, png) VALUES (?, ?, ?)" (i, hash, png)
  AvatarDropped hash -> execute db "DELETE FROM avatars WHERE profile = ? AND hash = ?" (i, hash)
  ContactChanged (ContactId key) wanted contact ->
    execute db
      "INSERT OR REPLACE INTO contacts (profile, key, name, unread, request_address, request_message,\
      \ status_message, wanted, muted, muted_until, avatar, send_pictures) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ( Only i
      :. ( key, contactName contact, contactUnread contact
        , fst <$> contactRequest contact, snd <$> contactRequest contact )
      :. ( contactStatusMessage contact, wanted, isJust (contactMute contact), mutedUntil (contactMute contact)
         , contactAvatar contact )
      :. Only (contactPictures contact) )
  -- Deleted with their chats, so delete those and their files too.
  ContactDeleted (ContactId key) -> do
    paths <- SQLite.query db "SELECT file_path FROM messages WHERE profile = ? AND contact = ? AND file_path IS NOT NULL" (i, key)
    forM_ paths $ \(Only path) -> void (try (removeFile path) :: IO (Either IOException ()))
    execute db "DELETE FROM messages WHERE profile = ? AND contact = ?" (i, key)
    execute db "DELETE FROM contacts WHERE profile = ? AND key = ?" (i, key)
  -- New ones go after the others in their chat.
  MessageChanged (ContactId key) m ->
    execute db
      "INSERT INTO messages (profile, contact, place, mine, text, time, delivery,\
      \ file_size, file_path, file_done, file_state, call_video, call_duration, call_missed, parts, file_id,\
      \ kind, raw, delivered_time, id) VALUES (?, ?, (SELECT COALESCE(MAX(place) + 1, 0) FROM messages\
      \ WHERE profile = ? AND contact = ?), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\
      \ ON CONFLICT (profile, id) DO UPDATE SET mine = excluded.mine,\
      \ text = excluded.text, time = excluded.time, delivery = excluded.delivery,\
      \ file_size = excluded.file_size, file_path = excluded.file_path, file_done = excluded.file_done,\
      \ file_state = excluded.file_state, call_video = excluded.call_video,\
      \ call_duration = excluded.call_duration, call_missed = excluded.call_missed, parts = excluded.parts,\
      \ file_id = excluded.file_id, kind = excluded.kind, raw = excluded.raw,\
      \ delivered_time = excluded.delivered_time"
      ( (i, key, i, key)
      :. ( messageMine m, messageText m, toMillis (messageTime m), deliveryName (messageDelivery m) )
      :. ( attachmentSize <$> messageFile m, attachmentPath <$> messageFile m
         , attachmentDone <$> messageFile m, fileStateName . attachmentState <$> messageFile m )
      :. ( recordVideo <$> messageCall m, fmap toMillis' . recordDuration =<< messageCall m
         , recordMissed <$> messageCall m, messageParts m, attachmentId =<< messageFile m )
      :. ( if messageKind m == Tox.MessageAction then 1 else 0 :: Int, messageRaw m
         , toMillis <$> messageDelivered m, idBytes (messageId m) ) )
  GroupChanged (GroupId chat) g draft -> do
    execute db
      "INSERT OR REPLACE INTO groups (profile, chat_id, name, phase, unread, saved, muted, muted_until, draft,\
      \ mentions, topic, kind, pictures) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ( (i, chat, groupName g, phaseName (groupPhase g), groupUnread g)
      :. (groupSaved g, isJust (groupMute g), mutedUntil (groupMute g), draft, groupMentions g, groupTopic g)
      :. (kindName (groupKind g), groupPictures g) )
    case groupRelayBot g of
      Just bot -> execute db "INSERT OR REPLACE INTO relay_bots (profile, chat_id, bot) VALUES (?, ?, ?)" (i, chat, bot)
      Nothing  -> execute db "DELETE FROM relay_bots WHERE profile = ? AND chat_id = ?" (i, chat)
  GroupDeleted (GroupId chat) -> do
    execute db "DELETE FROM group_entries WHERE profile = ? AND chat_id = ?" (i, chat)
    execute db "DELETE FROM relay_bots WHERE profile = ? AND chat_id = ?" (i, chat)
    execute db "DELETE FROM groups WHERE profile = ? AND chat_id = ?" (i, chat)
  EntryChanged (GroupId chat) (GroupEntry time happening eid) -> do
    let (what, member, kind, text, delivery, other) = case happening of
          Said who k t d _     -> ("said" :: Text, who, k, Just t, Just (deliveryName d), Nothing)
          SharedFile who _ _   -> ("shared file", who, Tox.MessageNormal, Nothing, Nothing, Nothing)
          PrivateFrom who k t  -> ("private from", Just who, k, Just t, Nothing, Nothing)
          PrivateTo who k t d  -> ("private to", Just who, k, Just t, Just (deliveryName d), Nothing)
          Moderated by whom m  -> ("moderated", by, Tox.MessageNormal, Just (moderationName m), Nothing, whom)
          Arrived who          -> ("arrived", Just who, Tox.MessageNormal, Nothing, Nothing, Nothing)
          Departed who         -> ("departed", Just who, Tox.MessageNormal, Nothing, Nothing, Nothing)
          TopicSet who t       -> ("topic", who, Tox.MessageNormal, Just t, Nothing, Nothing)
          Noted t              -> ("noted", Nothing, Tox.MessageNormal, Just t, Nothing, Nothing)
        (messageNumber, shared) = case happening of
          Said _ _ _ _ mid      -> (mid, Nothing)
          SharedFile _ file mid -> (mid, Just file)
          _                     -> (Nothing, Nothing)
        attachment = groupFileData =<< shared
    place <- entryPlace db i chat time eid
    execute db
      "INSERT INTO group_entries (profile, chat_id, place, time, what, member_key, member_name,\
      \ kind, text, delivery, other_key, other_name, id, tox_id, file_id, file_info, file_size, file_path,\
      \ file_done, file_state) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\
      \ ON CONFLICT (profile, id) DO UPDATE SET time = excluded.time,\
      \ what = excluded.what, member_key = excluded.member_key, member_name = excluded.member_name,\
      \ kind = excluded.kind, text = excluded.text, delivery = excluded.delivery,\
      \ other_key = excluded.other_key, other_name = excluded.other_name, tox_id = excluded.tox_id,\
      \ file_id = excluded.file_id, file_info = excluded.file_info, file_size = excluded.file_size,\
      \ file_path = excluded.file_path, file_done = excluded.file_done, file_state = excluded.file_state"
      ( (i, chat, place, toMillis time, what)
      :. ( memberKey <$> member, memberName <$> member
         , if kind == Tox.MessageAction then 1 else 0 :: Int, text, delivery )
      :. (memberKey <$> other, memberName <$> other, idBytes eid)
      :. (fromIntegral <$> messageNumber :: Maybe Int64, groupFileId <$> shared, Info.encode <$> (groupFileInfo =<< shared), attachmentSize <$> attachment)
      :. (attachmentPath <$> attachment, attachmentDone <$> attachment, fileStateName . attachmentState <$> attachment) )
  InviteChanged (ContactId friend) (GroupId chat) invite ->
    execute db
      "INSERT OR REPLACE INTO invites (profile, friend, chat_id, name, data, answered, password, kind)\
      \ VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      ( i, friend, chat, inviteName invite, inviteData invite, isJust (inviteAnswered invite), inviteAnswered invite
      , kindName (inviteKind invite) )
  InviteDeleted (ContactId friend) (GroupId chat) ->
    execute db "DELETE FROM invites WHERE profile = ? AND friend = ? AND chat_id = ?" (i, friend, chat)
  StagedChanged (ContactId key) items -> do
    execute db "DELETE FROM staged WHERE profile = ? AND contact = ?" (i, key)
    forM_ (zip [0 :: Int ..] items) $ \(place, item) ->
      execute db "INSERT INTO staged (profile, contact, place, path, name, size, file_id, latitude, longitude)\
                 \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ( (i, key, place)
        :. case item of
             StagedFile path name size fileId ->
               (Just path, Just name, Just size, Just fileId, Nothing, Nothing)
             StagedPlace latitude longitude ->
               (Nothing, Nothing, Nothing, Nothing, Just latitude, Just longitude) )
  TextChanged texts (ContactId key) (Just text) ->
    execute db (SQLite.Query ("INSERT OR REPLACE INTO " <> tableOf texts <> " (profile, key, message) VALUES (?, ?, ?)"))
      (i, key, text)
  TextChanged texts (ContactId key) Nothing ->
    execute db (SQLite.Query ("DELETE FROM " <> tableOf texts <> " WHERE profile = ? AND key = ?")) (i, key)
  PairingChanged pairing -> execute db "UPDATE profiles SET pairing = ? WHERE id = ?" (pairing, i)
  EventAdded (Sync.EventId key) rank bytes ->
    execute db "INSERT OR IGNORE INTO sync_events (profile, id, rank, bytes) VALUES (?, ?, ?, ?)" (i, key, rank, bytes)
  where
    mutedUntil = \case
      Just (MutedUntil end) -> Just (toMillis end)
      _                     -> Nothing
    idBytes (MessageId bytes) = bytes
    tableOf = \case
      Requests -> "requests"
      Declined -> "declined"
      Drafts   -> "drafts"

-- | A saved status, or online if the number isn't one.
-- | Where a group entry goes: its own place if it has one, else before the
-- first newer one, as entries from history are older than what we have.
entryPlace :: SQLite.Connection -> Int64 -> Text -> UTCTime -> MessageId -> IO Int64
entryPlace db i chat time (MessageId eid) = SQLite.query db "SELECT place FROM group_entries WHERE profile = ? AND id = ?" (i, eid) >>= \case
  [Only place] -> pure place
  _ -> SQLite.query db "SELECT place, time FROM group_entries WHERE profile = ? AND chat_id = ? ORDER BY place DESC LIMIT 1" (i, chat) >>= \case
    [] -> pure 0
    (newest, newestTime) : _
      | newestTime <= (toMillis time :: Integer) -> pure (newest + 1)
      | otherwise -> do
          [Only place] <- SQLite.query db "SELECT MIN(place) FROM group_entries WHERE profile = ? AND chat_id = ? AND time > ?" (i, chat, toMillis time)
          -- Twice, as each row must stay unique on the way.
          execute db "UPDATE group_entries SET place = -place - 1 WHERE profile = ? AND chat_id = ? AND place >= ?" (i, chat, place)
          execute db "UPDATE group_entries SET place = -place WHERE profile = ? AND chat_id = ? AND place < 0" (i, chat)
          pure place

awayOf :: Int -> Tox.UserStatus
awayOf n = if n >= 0 && n <= fromEnum (maxBound :: Tox.UserStatus) then toEnum n else Tox.Online

toMillis :: UTCTime -> Integer
toMillis = toMillis' . utcTimeToPOSIXSeconds

fromMillis :: Integer -> UTCTime
fromMillis = posixSecondsToUTCTime . fromMillis'

toMillis' :: NominalDiffTime -> Integer
toMillis' = round . (* 1000)

fromMillis' :: Integer -> NominalDiffTime
fromMillis' = (/ 1000) . fromIntegral

-- | Settings by name: those without a row have their defaults.
type Named = Map Text SQLite.SQLData

accountSettings :: Named -> AccountSettings
accountSettings named = AccountSettings
  { settingAutoAccept = maybe (settingAutoAccept defaults) fromIntegral (integerIn named "auto_accept")
  , settingTyping = maybe (settingTyping defaults) (/= 0) (integerIn named "typing")
  }
  where defaults = clientAccount initialClient

accountRows :: AccountSettings -> [(Text, SQLite.SQLData)]
accountRows account =
  [ ("auto_accept", integer (settingAutoAccept account))
  , ("typing", integer (fromEnum (settingTyping account)))
  ]

deviceSettings :: Named -> DeviceSettings
deviceSettings named = DeviceSettings
  { settingUdp = flag settingUdp "udp"
  , settingIpv6 = flag settingIpv6 "ipv6"
  , settingLan = flag settingLan "lan"
  , settingStay = flag settingStay "stay_online"
  , settingProxy = Tox.Proxy <$> (proxyKind =<< textIn named "proxy_type") <*> (Text.unpack <$> textIn named "proxy_host")
      <*> (fromIntegral <$> integerIn named "proxy_port")
  }
  where flag field name = maybe (field (clientDevice initialClient)) (/= 0) (integerIn named name)

-- | Each setting's row, none for one without a value.
deviceRows :: DeviceSettings -> [(Text, Maybe SQLite.SQLData)]
deviceRows device =
  [ ("udp", flag settingUdp), ("ipv6", flag settingIpv6), ("lan", flag settingLan), ("stay_online", flag settingStay)
  , ("proxy_type", SQLite.SQLText . proxyKindName . Tox.proxyType <$> proxy)
  , ("proxy_host", SQLite.SQLText . Text.pack . Tox.proxyHost <$> proxy)
  , ("proxy_port", integer . Tox.proxyPort <$> proxy)
  ]
  where
    flag field = Just (integer (fromEnum (field device)))
    proxy = settingProxy device

integer :: Integral a => a -> SQLite.SQLData
integer = SQLite.SQLInteger . fromIntegral

integerIn :: Named -> Text -> Maybe Integer
integerIn named name = case Map.lookup name named of
  Just (SQLite.SQLInteger n) -> Just (toInteger n)
  _                          -> Nothing

textIn :: Named -> Text -> Maybe Text
textIn named name = case Map.lookup name named of
  Just (SQLite.SQLText t) -> Just t
  _                       -> Nothing

proxyKindName :: Tox.ProxyType -> Text
proxyKindName = \case
  Tox.HttpProxy    -> "http"
  Tox.Socks5Proxy  -> "socks5"
  Tox.GatewayProxy -> "gateway"

proxyKind :: Text -> Maybe Tox.ProxyType
proxyKind = \case
  "http"    -> Just Tox.HttpProxy
  "socks5"  -> Just Tox.Socks5Proxy
  "gateway" -> Just Tox.GatewayProxy
  _        -> Nothing

kindOf :: Text -> GroupKind
kindOf = \case
  "conference"       -> TextConference
  "audio conference" -> AudioConference
  _                  -> NgcGroup

phaseName :: GroupPhase -> Text
phaseName = \case
  Joining                     -> "in"
  InGroup                     -> "in"
  Refused Tox.JoinFailFull    -> "full"
  Refused Tox.JoinFailBadPassword -> "password"
  Refused Tox.JoinFailUnknown -> "refused"
  Gone                        -> "gone"
  Kicked                      -> "kicked"

-- | Groups we were in are 'Joining' again after a restart.
phaseOf :: Text -> GroupPhase
phaseOf = \case
  "full"     -> Refused Tox.JoinFailFull
  "password" -> Refused Tox.JoinFailBadPassword
  "refused"  -> Refused Tox.JoinFailUnknown
  "gone"     -> Gone
  "kicked"   -> Kicked
  _          -> Joining

moderationName :: Tox.Moderation -> Text
moderationName = \case
  Tox.ModKick      -> "kick"
  Tox.ModObserver  -> "observer"
  Tox.ModUser      -> "user"
  Tox.ModModerator -> "moderator"

moderationOf :: Text -> Maybe Tox.Moderation
moderationOf = \case
  "kick"      -> Just Tox.ModKick
  "observer"  -> Just Tox.ModObserver
  "user"      -> Just Tox.ModUser
  "moderator" -> Just Tox.ModModerator
  _           -> Nothing

deliveryName :: Delivery -> Text
deliveryName delivery = case delivery of
  Pending   -> "pending"
  Sent      -> "sent"
  Delivered -> "delivered"

fileStateName :: FileState -> Text
fileStateName state = case state of
  Waiting     -> "waiting"
  Moving      -> "moving"
  Complete    -> "complete"
  Interrupted -> "interrupted"
  Cancelled   -> "cancelled"

fileState' :: Text -> FileState
fileState' name = case name of
  "waiting"     -> Waiting
  "moving"      -> Moving
  "complete"    -> Complete
  "interrupted" -> Interrupted
  -- Also "stopped", from before resuming.
  _             -> Cancelled

delivery' :: Text -> Delivery
delivery' name = case name of
  "sent"      -> Sent
  "delivered" -> Delivered
  _           -> Pending
