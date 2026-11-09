{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module HTox.StoreSpec (spec) where

import           Control.Concurrent      (forkIO, killThread)
import           Control.Exception       (bracket)
import           Control.Monad           (foldM_, void, (<=<))
import qualified Crypto.Saltine.Class    as Sodium
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Base16
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (listToMaybe)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Database.SQLite.Simple  as SQLite
import           System.Directory        (getTemporaryDirectory,
                                          removePathForcibly)
import           System.FilePath         ((</>))
import           System.Posix.Files      (fileMode, getFileStatus, groupModes,
                                          intersectFileModes, nullFileMode,
                                          otherModes, unionFileModes)
import           System.Timeout          (timeout)
import           Test.Hspec

import qualified Tox.Application.Tox     as Tox
import           Tox.Client              (initialClient)
import qualified Tox.Client.Home         as Home
import           Tox.Client.Types        (Ids (..), MessageId (..))
import qualified Tox.Client.Update       as Client
import qualified Tox.Crypto.Core.KeyPair as KeyPair
import qualified Tox.Share.History       as History
import qualified Tox.Share.Info          as Info
import qualified Tox.Sync.Id             as Sync
import qualified Tox.Sync.Store          as Sync

import           HTox.Model
import           HTox.Sample
import           HTox.Store

-- | Store of the active account, with each event saved as it happens.
saved :: [Event] -> IO Store
saved events = open Nothing ":memory:" >>= \database -> savedIn database events

savedIn :: Database -> [Event] -> IO Store
savedIn database events = do
  store <- profileStore database <$> activeProfile database
  foldM_ (\old event -> let new = fst (update event old) in new <$ save store (modelClient old) (modelClient new))
    initial events
  pure store

-- | As version 3 left it: settings in columns, the device's in one row.
asBeforeSettings :: SQLite.Connection -> IO ()
asBeforeSettings db = mapM_ (SQLite.execute_ db) $
  [ "ALTER TABLE groups DROP COLUMN pictures"
  , "ALTER TABLE profiles ADD COLUMN auto_accept INTEGER NOT NULL DEFAULT 0"
  , "ALTER TABLE profiles ADD COLUMN formats_off TEXT NOT NULL DEFAULT ''"
  , "UPDATE profiles SET auto_accept = ifnull((" <> setting "auto_accept" <> "), 4194304)"
  , "DROP TABLE account_settings" ]
  ++ [ "ALTER TABLE device ADD COLUMN " <> column <> " " <> kind | (column, kind) <- deviceColumns ]
  ++ [ "UPDATE device SET " <> column <> " = (SELECT value FROM device_settings WHERE name = '" <> column <> "')"
     | (column, _) <- deviceColumns ]
  ++ [ "DROP TABLE device_settings", "ALTER TABLE device RENAME TO device_settings", "PRAGMA user_version = 3" ]
  where
    setting name = "SELECT value FROM account_settings WHERE profile = profiles.id AND name = '" <> name <> "'"
    deviceColumns =
      [ ("udp", "INTEGER"), ("ipv6", "INTEGER"), ("lan", "INTEGER"), ("stay_online", "INTEGER")
      , ("proxy_type", "TEXT"), ("proxy_host", "TEXT"), ("proxy_port", "INTEGER") ]

spec :: Spec
spec = do
  it "keeps the profile, friends, requests and chats" $ do
    model <- load =<< saved history
    let client = modelClient sample
        pid = clientProfileId (modelClient model)
        -- The store made the profile's own.
        seed = idsSeed (clientIds (modelClient model))
    BS.length seed `shouldBe` 32
    model `shouldBe` sample
      { modelClient = client
          { clientProfile = (clientProfile client) { profileToxId = "" }
          , clientProfileId = pid
          , clientIds = (clientIds client) { idsSeed = seed }
          , clientConnection = Offline
          , clientContacts = (\c -> c { contactOnline = False }) <$> clientContacts client
          }
      , modelProfiles = [Account pid "Tox User" ""]
      , modelJump = 0
      }

  it "keeps our pending request and the rejected ones" $ do
    let address = toxId (Text.replicate 8 "0123ABCD") "DEADBEEF"
        events = history ++ [Open AddContact, AddressChanged address, AddFriend, ClientEvent (Client.RejectRequest carol)]
    model <- load =<< saved events
    contactRequest <$> Map.lookup (ContactId (Text.take 64 address)) (clientContacts (modelClient model))
      `shouldBe` Just (Just (address, "Hey, I'm Tox User, come chat with me on Tox!"))
    Map.keys (clientDeclined (modelClient model)) `shouldBe` [carol]

  it "keeps status messages, mutes, and chats kept of deleted contacts" $ do
    let events = history ++
          [ ClientEvent (Client.FriendStatusMessage alice "Busy day"), ClientEvent (Client.MuteChat alice (Just (at 9 0 0)))
          , Open (Chat bob), DraftChanged "hi" (at 1 0 0), Send (at 3 9 0), ClientEvent (Client.DeleteContact bob True) ]
    model <- load =<< saved events
    (contactStatusMessage <$> Map.lookup alice (clientContacts (modelClient model))) `shouldBe` Just "Busy day"
    (contactMute <$> Map.lookup alice (clientContacts (modelClient model))) `shouldBe` Just (Just (MutedUntil (at 9 0 0)))
    map messageText . contactMessages <$> Map.lookup bob (clientFormer (modelClient model)) `shouldBe` Just ["hi"]
    gone <- load =<< saved (events ++ [ClientEvent (Client.DeleteContact alice False)])
    Map.member alice (clientContacts (modelClient gone)) || Map.member alice (clientFormer (modelClient gone)) `shouldBe` False

  it "keeps their messages as Tox carried them" $ do
    let bytes = "hi" <> BS.pack (0 : 0 : replicate 36 1)
    model <- load =<< saved (history ++ [ClientEvent (Client.Received alice (at 3 9 0) Tox.MessageAction bytes)])
    [ (messageKind m, messageRaw m) | Just c <- [Map.lookup alice (clientContacts (modelClient model))], m <- drop 3 (contactMessages c) ]
      `shouldBe` [(Tox.MessageAction, Just bytes)]

  it "keeps when our message was delivered" $ do
    model <- load =<< saved history
    (messageDelivered <=< listToMaybe . drop 1 . contactMessages <=< Map.lookup alice) (clientContacts (modelClient model))
      `shouldBe` Just (at 1 15 16)

  it "keeps settings, new ones taking the default file size" $ do
    fresh <- load =<< saved history
    settingAutoAccept (clientAccount (modelClient fresh)) `shouldBe` 4 * 1024 * 1024
    let device = DeviceSettings False True False False (Just (Tox.Proxy Tox.Socks5Proxy "127.0.0.1" 9050))
    settingTyping (clientAccount (modelClient fresh)) `shouldBe` True
    model <- load =<< saved (history ++ map ClientEvent
      [Client.SetAutoAccept 0, Client.SetDevice device, Client.SetTypingIndicators False])
    (clientAccount (modelClient model), clientDevice (modelClient model))
      `shouldBe` (AccountSettings 0 False, device)

  it "keeps the pictures we and friends show" $ do
    model <- load =<< saved (history ++ [ClientEvent (Client.SetAvatar "ours"), ClientEvent (Client.FriendAvatar alice (Just "theirs"))])
    ((`Map.lookup` clientAvatars (modelClient model)) =<< clientAvatar (modelClient model)) `shouldBe` Just "ours"
    ((`Map.lookup` clientAvatars (modelClient model)) =<< contactAvatar =<< Map.lookup alice (clientContacts (modelClient model))) `shouldBe` Just "theirs"

  it "keeps drafts" $ do
    model <- load =<< saved (history ++ [Open (Chat bob), DraftChanged "later" (at 1 0 0)])
    Map.lookup bob (clientDrafts (modelClient model)) `shouldBe` Just "later"

  it "keeps how far a message got" $ do
    let sent = history ++ [Open (Chat bob), DraftChanged "hi" (at 1 0 0), Send (at 3 9 0)]
        events = sent ++ [ClientEvent (Client.Delivering bob (messageAt bob 0 (applying sent initial)) Sent)]
    model <- load =<< saved events
    map messageDelivery . contactMessages <$> Map.lookup bob (clientContacts (modelClient model))
      `shouldBe` Just [Sent]

  it "keeps how many parts of a message the friend has" $ do
    let sent = history ++ [Open (Chat bob), DraftChanged (Text.replicate 3000 "a") (at 1 0 0), Send (at 3 9 0)]
        events = sent ++ [ClientEvent (Client.Confirmed bob (messageAt bob 0 (applying sent initial)) 2 (at 3 9 1))]
    model <- load =<< saved events
    map messageParts . contactMessages <$> Map.lookup bob (clientContacts (modelClient model)) `shouldBe` Just [2]

  it "keeps each message's ID, which stays when the message changes" $ do
    db <- SQLite.open ":memory:"
    database <- opened db
    let sent = history ++ [Open (Chat bob), DraftChanged "hi" (at 1 0 0), Send (at 3 9 0), DraftChanged "again" (at 1 0 0), Send (at 3 9 1)]
        ContactId key = bob
        ids = flush database >> map (MessageId . SQLite.fromOnly) <$> SQLite.query db "SELECT id FROM messages WHERE contact = ? ORDER BY place" (SQLite.Only key)
        made' = [messageAt bob n (applying sent initial) | n <- [0, 1]]
    _ <- savedIn database sent
    ids `shouldReturn` made'
    _ <- savedIn database (sent ++ [ClientEvent (Client.Delivering bob (messageAt bob 0 (applying sent initial)) Sent)])
    ids `shouldReturn` made'

  it "moves settings from columns into rows by name, keeping them" $ do
    db <- SQLite.open ":memory:"
    database <- opened db
    let device = DeviceSettings False True False False (Just (Tox.Proxy Tox.Socks5Proxy "127.0.0.1" 9050))
    old <- load =<< savedIn database (history ++ map ClientEvent
      [Client.SetAutoAccept 0, Client.SetDevice device])
    asBeforeSettings db
    model <- load . flip profileStore (clientProfileId (modelClient old)) =<< opened db
    (clientAccount (modelClient model), clientDevice (modelClient model))
      `shouldBe` (AccountSettings 0 True, device)

  it "gives a database from before message IDs its seed, keeping the chats" $ do
    db <- SQLite.open ":memory:"
    database <- opened db
    old <- load =<< savedIn database history
    -- As version 1 left it.
    asBeforeSettings db
    mapM_ (SQLite.execute_ db)
      ( ["ALTER TABLE profiles DROP COLUMN id_seed", "ALTER TABLE profiles DROP COLUMN ids_used"]
        ++ [ "ALTER TABLE group_entries DROP COLUMN " <> column
           | column <- ["tox_id", "file_id", "file_info", "file_size", "file_path", "file_done", "file_state"] ]
        ++ ["PRAGMA user_version = 1"] )
    store <- flip profileStore (clientProfileId (modelClient old)) <$> opened db
    model <- load store
    BS.length (idsSeed (clientIds (modelClient model))) `shouldBe` 32
    settingTyping (clientAccount (modelClient model)) `shouldBe` True
    clientContacts (modelClient model) `shouldBe` clientContacts (modelClient old)
    save store (modelClient model) (modelClient (applying [Open (Chat alice), DraftChanged "after" (at 1 0 0), Send (at 3 9 0)] model))
    later <- load store
    map messageText . contactMessages <$> Map.lookup alice (clientContacts (modelClient later))
      `shouldBe` Just ["Hi! Is this the Haskell client?", "It is, and it's on the network now.", "Looks good already.", "after"]

  it "loads each chat's newest and what is still in use, and earlier pages when asked" $ do
    let many cid = [ClientEvent (Client.Received cid (at 4 0 n) Tox.MessageNormal "more") | n <- [1 .. fromIntegral chatPage + 5]]
        -- Ours waits for Bob, so all after it come along.
        events = history ++ [Open (Chat bob), DraftChanged "waiting" (at 1 0 0), Send (at 3 9 0), Back] ++ many alice ++ many bob
        full = modelClient (applying events initial)
        chatOf cid = maybe [] contactMessages . Map.lookup cid . clientContacts
    store <- saved events
    loaded <- modelClient <$> load store
    length (chatOf alice loaded) `shouldBe` chatPage
    chatOf bob loaded `shouldBe` chatOf bob full
    (contactEarlier <$> Map.lookup alice (clientContacts loaded), contactEarlier <$> Map.lookup bob (clientContacts loaded))
      `shouldBe` (Just True, Just False)
    let more client = case chatOf alice client of
          oldest : _ -> (\e -> fst (Client.update (Client.Viewing Nothing True True chatPage) e client))
            <$> older store (Client.FriendChat alice) (messageId oldest)
          [] -> pure client
    paged <- more loaded
    map messageText (chatOf alice paged) `shouldBe`
      ["Hi! Is this the Haskell client?", "It is, and it's on the network now.", "Looks good already."] ++ replicate (chatPage + 5) "more"
    end <- more paged
    contactEarlier <$> Map.lookup alice (clientContacts end) `shouldBe` Just False

  it "refuses a database of a newer version, leaving it" $ do
    db <- SQLite.open ":memory:"
    SQLite.execute_ db "PRAGMA user_version = 99"
    void (opened db) `shouldThrow` (\(Newer v) -> v == 99)
    SQLite.query_ db "PRAGMA user_version" `shouldReturn` [SQLite.Only (99 :: Int)]

  it "keeps files, and stops those a restart cut off" $ do
    let events = history ++
          [ Open (Chat alice), Attach (at 3 8 0) "/sent/a.png" "a.png" 10 "ours", Send (at 3 9 0)
          , ClientEvent (Client.FileOffered alice (Receiving 1) (at 3 9 1) "b.txt" 20 "/received/b.txt" "theirs")
          , ClientEvent (Client.FileMoved alice (Receiving 1) 5) ]
    model <- load =<< saved events
    [ (messageText m, f) | m <- maybe [] contactMessages (Map.lookup alice (clientContacts (modelClient model))), Just f <- [messageFile m] ]
      `shouldBe` [ ("a.png", Attachment 10 "/sent/a.png" 0 Waiting (Just "ours"))
                 , ("b.txt", Attachment 20 "/received/b.txt" 5 Interrupted (Just "theirs")) ]

  it "keeps files shared in groups, Tox message IDs and how pictures go, and stops a download a restart cut off" $ do
    let dana = Member (Text.replicate 8 "DA7A0000") "Dana"
        inf = Info.Info "cat.png" 1000 1000 (BS.replicate 20 1)
        fid = Info.hash inf
        events = history ++
          [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
          , ClientEvent (Client.MemberJoined team (at 3 9 0) 1 dana), ClientEvent (Client.GroupConnected team (at 3 9 0))
          , ClientEvent (Client.GroupReceived team (at 3 9 1) 1 Tox.MessageNormal "look" (Just 5))
          , ClientEvent (Client.GroupFileShared team (at 3 9 2) 1 6 fid), ClientEvent (Client.GroupFileLearned team fid inf)
          , ClientEvent (Client.DownloadGroupFile team fid), ClientEvent (Client.GroupFileMoved team fid "/received/cat.png" 500 0)
          , ClientEvent (Client.SendGroupPictures team (Just "qoi")) ]
    model <- load =<< saved events
    groupPictures <$> Map.lookup team (clientGroups (modelClient model)) `shouldBe` Just (Just "qoi")
    map entryWhat . groupEntries <$> Map.lookup team (clientGroups (modelClient model))
      `shouldBe` Just [ Said (Just dana) Tox.MessageNormal "look" Delivered (Just 5)
                      , SharedFile (Just dana) (GroupFile fid (Just inf) (Just (Attachment 1000 "/received/cat.png" 500 Interrupted (Just fid))) 0) (Just 6) ]

  it "puts history from a member where it belongs in time, and leaves out what it has" $ do
    let dana = Member (Text.replicate 8 "DA7A0000") "Dana"
        early = (at 3 9 2, Just (memberKey dana), 3, History.Text False "early")
        late = (at 3 9 5, Just (memberKey dana), 5, History.Text False "late")
        events = history ++
          [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
          , ClientEvent (Client.MemberJoined team (at 3 9 0) 1 dana), ClientEvent (Client.GroupConnected team (at 3 9 0))
          , ClientEvent (Client.GroupReceived team (at 3 9 5) 1 Tox.MessageNormal "late" (Just 5))
          , ClientEvent (Client.GroupRecalled team (at 3 9 6) 1 Map.empty [early]) ]
    store <- saved events
    model <- load store
    map entryWhat . groupEntries <$> Map.lookup team (clientGroups (modelClient model))
      `shouldBe` Just [ Said (Just dana) Tox.MessageNormal "early" Delivered (Just 3)
                      , Said (Just dana) Tox.MessageNormal "late" Delivered (Just 5) ]
    recalled store (Client.GroupRecalled team (at 3 9 6) 1 Map.empty [early, late]) >>= \case
      Client.GroupRecalled _ _ _ _ left -> left `shouldBe` []
      _ -> expectationFailure "not history"
    -- Dana may be gone by then, but the chat knew her name.
    recalled store (Client.GroupRecalled team (at 3 9 6) 1 Map.empty [(at 3 9 7, Just (memberKey dana), 9, History.Text False "new")]) >>= \case
      Client.GroupRecalled _ _ _ names _ -> names `shouldBe` Map.fromList [(memberKey dana, "Dana")]
      _ -> expectationFailure "not history"

  it "keeps what is picked for a draft" $ do
    let staged = [StagedFile "/sent/a.png" "a.png" 10 "ours", StagedPlace 1.5 2.5]
    model <- load =<< saved (history ++ [Open (Chat alice), Attach (at 3 8 0) "/sent/a.png" "a.png" 10 "ours", ShareLocation 1.5 2.5])
    Map.lookup alice (clientStaged (modelClient model)) `shouldBe` Just staged

  it "keeps calls" $ do
    let events = history ++
          [ Open (Chat alice), StartCall (at 3 9 0) True, ClientEvent (Client.CallPeer alice (at 3 9 0) True), ClientEvent (Client.HangUp (at 3 9 2))
          , ClientEvent (Client.CallInvited alice (at 3 9 3) False), ClientEvent (Client.CallEnded alice (at 3 9 4)) ]
    model <- load =<< saved events
    [ c | m <- maybe [] contactMessages (Map.lookup alice (clientContacts (modelClient model))), Just c <- [messageCall m] ]
      `shouldBe` [CallRecord True (Just 120) False, CallRecord False Nothing True]

  it "keeps each account's chats apart, and which is online" $ do
    database <- open Nothing ":memory:"
    first <- activeProfile database
    _ <- savedIn database history
    second <- newProfile database Nothing (clientProfile initialClient) { profileNickname = "Other" }
    setActive database second
    other <- load =<< savedIn database [ClientEvent (Client.Requested carol "hi")]
    (Map.keys (clientContacts (modelClient other)), Map.keys (clientRequests (modelClient other))) `shouldBe` ([], [carol])
    map accountName (modelProfiles other) `shouldBe` ["Tox User", "Other"]
    activeProfile database `shouldReturn` second
    deleteProfile database first
    map accountId <$> profiles database `shouldReturn` [second]
    Map.keys . clientContacts . modelClient <$> load (profileStore database first) `shouldReturn` []

  it "keeps groups with their chats, and invites" $ do
    let dana = Member (Text.replicate 8 "DA7A0000") "Dana"
        events = history ++
          [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
          , ClientEvent (Client.MemberJoined team (at 3 9 0) 1 dana), ClientEvent (Client.GroupConnected team (at 3 9 0)), ClientEvent (Client.GroupSaved team "state")
          , ClientEvent (Client.GroupReceived team (at 3 9 1) 1 Tox.MessageAction "waves" Nothing), DraftChanged "later" (at 1 0 0)
          , ClientEvent (Client.TopicChanged team (at 3 9 2) (Just 1) "plans"), ClientEvent (Client.TopicSetByUs team (at 3 9 3) "ours")
          , ClientEvent (Client.MuteGroup team Nothing), ClientEvent (Client.ToggleMentions team), ClientEvent (Client.ChooseRelayBot team (Just (memberKey dana)))
          , ClientEvent (Client.PrivateReceived team (at 3 9 4) 1 Tox.MessageNormal "psst")
          , ClientEvent (Client.GroupModerated team (at 3 9 5) Nothing (Just 1) Tox.ModObserver)
          , ClientEvent (Client.GroupModerated team (at 3 9 6) (Just 1) Nothing Tox.ModKick)
          , ClientEvent (Client.Invited bob (GroupId "C0DE") NgcGroup "Other" "other invite")
          , ClientEvent (Client.Invited bob (GroupId "01AB") AudioConference "" "cookie")
          , ClientEvent (Client.Invited alice (GroupId "00CD") TextConference "" "text cookie"), AnswerInvite alice (GroupId "00CD") True ]
        expected = fst (update (ClientEvent (Client.Started "")) (applying events initial))
    model <- load =<< saved events
    clientGroups (modelClient model) `shouldBe` clientGroups (modelClient expected)
    clientInvites (modelClient model) `shouldBe` clientInvites (modelClient expected)
    clientGroupDrafts (modelClient model) `shouldBe` Map.fromList [(team, "later")]

  it "keeps the events of a home with devices, which it is again when loaded" $ do
    database <- open Nothing ":memory:"
    store <- profileStore database <$> activeProfile database
    keys <- KeyPair.newKeyPair
    saveIdentity store (Identity (Sodium.encode (KeyPair.secretKey keys)) 0x0BADF00D)
    let key = Text.toUpper (Text.decodeUtf8 (Base16.encode (Sodium.encode (KeyPair.publicKey keys))))
        events = ClientEvent (Client.Started (toxId key "0BADF00D")) : drop 1 history
          ++ [ ClientEvent (Client.Pair "c0de" (at 3 9 0))
             , ClientEvent (Client.Requested (ContactId (Text.replicate 32 "DD")) "pair:c0de") ]
        held = fmap (Sync.storedIds . Home.homeStore) . clientHome . modelClient
    _ <- savedIn database events
    model <- load store
    held model `shouldBe` held (applying events initial)
    fmap (Map.keys . Home.devices) (clientHome (modelClient model)) `shouldBe` Just [Sync.PublicKey (BS.replicate 32 0xdd)]

  it "keeps a linked account's settings and what waits to go" $ do
    database <- open Nothing ":memory:"
    store <- profileStore database <$> newLinkedProfile database (Text.replicate 76 "A") "pairing" (at 3 9 0)
    linked <- load store
    let events = [ClientEvent (Client.SetAutoAccept 0), Open (Chat alice), ShareLocation 1 2]
    foldM_ (\old event -> let new = fst (update event old) in new <$ save store (modelClient old) (modelClient new)) linked events
    model <- load store
    (settingAutoAccept (clientAccount (modelClient model)), Map.lookup alice (clientStaged (modelClient model)))
      `shouldBe` (0, Just [StagedPlace 1 2])

  it "backs up the chosen profiles and restores those it doesn't have" $ do
      backup <- (</> "htox-backup-test.sqlite") <$> getTemporaryDirectory
      let events = history ++
            [ Open (Chat alice), Attach (at 3 8 0) "/sent/a.png" "a.png" 10 "ours", Send (at 3 9 0) ]
      here <- open Nothing ":memory:"
      first <- activeProfile here
      store <- savedIn here events
      saveIdentity store (Identity "key" 1)
      _ <- newProfile here Nothing (clientProfile initialClient) { profileNickname = "Other" }
      backUp here [first] backup
      restore here backup `shouldReturn` []
      phone <- open Nothing ":memory:"
      _ <- activeProfile phone
      [new] <- restore phone backup
      map accountName <$> profiles phone `shouldReturn` ["Tox User", "Tox User"]
      model <- load (profileStore phone new)
      identity (profileStore phone new) `shouldReturn` Just (Identity "key" 1)
      Map.keys (clientContacts (modelClient model)) `shouldBe` Map.keys (clientContacts (modelClient sample))
      -- The phone doesn't have our file.
      [ attachmentState f | m <- maybe [] contactMessages (Map.lookup alice (clientContacts (modelClient model))), Just f <- [messageFile m] ]
        `shouldBe` [Cancelled]

  it "writes what it saves in the background, before anything else uses the database" $ do
    path <- (</> "htox-queue-test.sqlite") <$> getTemporaryDirectory
    mapM_ (removePathForcibly . (path <>)) ["", "-wal", "-shm"]
    database <- open Nothing path
    void (savedIn database (history ++ [Open (Chat bob), DraftChanged "later" (at 1 0 0)]))
    let drafts = SQLite.withConnection path (`SQLite.query_` "SELECT message FROM drafts") :: IO [SQLite.Only Text.Text]
    drafts `shouldReturn` []
    void (activeProfile database)
    drafts `shouldReturn` [SQLite.Only "later"]

  it "lets Tox wait until what it would do is written" $ do
    database <- open Nothing ":memory:"
    void (savedIn database history)
    timeout 100000 (written database) `shouldReturn` Nothing
    bracket (forkIO (writing (const (pure ())) id database)) killThread $ \_ ->
      timeout 5000000 (written database) `shouldReturn` Just ()

  it "commits without waiting for the disk" $ do
    path <- (</> "htox-commit-test.sqlite") <$> getTemporaryDirectory
    void (open Nothing path)
    SQLite.withConnection path (`SQLite.query_` "PRAGMA journal_mode") `shouldReturn` [SQLite.Only ("wal" :: Text.Text)]

  it "writes a backup only its owner can read" $ do
    backup <- (</> "htox-private-test.sqlite") <$> getTemporaryDirectory
    here <- open Nothing ":memory:"
    first <- activeProfile here
    backUp here [first] backup
    mode <- fileMode <$> getFileStatus backup
    intersectFileModes mode (unionFileModes groupModes otherModes) `shouldBe` nullFileMode

  it "keeps whether we are away" $ do
    model <- load =<< saved (history ++ [ClientEvent (Client.SetAway Tox.Busy)])
    profileAway (clientProfile (modelClient model)) `shouldBe` Tox.Busy

  it "keeps who we are" $ do
    database <- open Nothing ":memory:"
    store <- profileStore database <$> activeProfile database
    identity store `shouldReturn` Nothing
    saveIdentity store (Identity "key" 0xDEADBEEF)
    identity store `shouldReturn` Just (Identity "key" 0xDEADBEEF)
