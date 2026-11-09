{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
-- | App state, the actions widgets fire, and the Tox behind them.
module HTox.App
  ( app
  ) where

import           Control.Concurrent        (ThreadId, forkIO, killThread,
                                            threadDelay)
import           Control.Concurrent.Chan   (newChan, readChan, writeList2Chan)
import           Control.Concurrent.MVar   (MVar, modifyMVar, modifyMVar_,
                                            newEmptyMVar, newMVar, readMVar,
                                            takeMVar, tryPutMVar, tryTakeMVar,
                                            withMVar)
import           Control.Exception         (Exception (..), IOException,
                                            SomeException (..), evaluate,
                                            onException, throwIO, try)
import           Control.Monad             (forM, forM_, forever, guard, join,
                                            unless, void, when, (<=<))
import           Crypto.Saltine.Core.Utils (randomByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as Base16
import qualified Data.ByteString.Internal  as BS
import           Data.Char                 (toLower)
import           Data.Int                  (Int16)
import           Data.IORef                (IORef, atomicModifyIORef',
                                            modifyIORef', newIORef, readIORef,
                                            writeIORef)
import           Data.List                 (find)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (catMaybes, fromMaybe, isJust,
                                            isNothing)
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Data.Text.IO              as Text
import           Data.Time                 (NominalDiffTime, UTCTime,
                                            addUTCTime, defaultTimeLocale,
                                            diffUTCTime, formatTime,
                                            getCurrentTime, getCurrentTimeZone,
                                            utctDay)
import           Data.Time.Clock.POSIX     (utcTimeToPOSIXSeconds)
import qualified Data.Vector.Storable      as V
import           Data.Word                 (Word64)
import           Foreign.ForeignPtr        (castForeignPtr)
import           GHC.Clock                 (getMonotonicTime)
import           GHC.Conc                  (labelThread, myThreadId)
import           Hatter                    (CallButton (..),
                                            LifecycleEvent (..), MobileApp (..),
                                            MobileContext (..), PickedFile (..),
                                            UserState (..), capturePicture,
                                            copyToClipboard, imageSize,
                                            is24HourClock, isDarkMode,
                                            loggingMobileContext, openFile,
                                            openUri, pickFiles, pickPicture,
                                            platformLog, saveFile)
import           Hatter.Action             (Action, ActionState,
                                            ListPosition (..), createAction,
                                            createOnChange, createOnScroll,
                                            newActionState, runActionM)
import           Hatter.BottomSheet        (BottomSheetAction (..),
                                            BottomSheetConfig (..),
                                            showBottomSheet)
import           Hatter.CallAudio          (CallAudio, CallRoute, CallTone (..),
                                            callAudioLatency, playCallAudio,
                                            setCallRoute, setCallTone,
                                            startCallAudio, stopCallAudio)
import qualified Hatter.CallAudio          as CallAudio
import           Hatter.CallVideo          (CallCamera, VideoFrame (..),
                                            showVideoFrame, startCallCamera,
                                            stopCallCamera)
import           Hatter.DatabaseKey        (DatabaseKey (..), databaseKey)
import           Hatter.DeviceInfo         (DeviceInfo (..), getDeviceInfo)
import           Hatter.Dialog             (DialogAction (..),
                                            DialogConfig (..), showDialog)
import           Hatter.FilesDir           (getAppFilesDir)
import           Hatter.Location           (LocationData (..),
                                            startLocationUpdates,
                                            stopLocationUpdates)
import           Hatter.Notification       (Notification (..),
                                            NotificationKind (..),
                                            allowFullScreen, cancelNotification,
                                            fullScreenAllowed, setKeepScreenOn,
                                            setOngoingCall, setRunInBackground,
                                            setServiceStatus,
                                            setShowOverLockScreen,
                                            showNotification, stopBackground)
import           Hatter.Permission         (Permission (..), PermissionState,
                                            PermissionStatus (..),
                                            requestPermission)
import           Hatter.Picture            (PictureKind (..), picturePixels,
                                            shrinkPicture)
import           System.Directory          (createDirectoryIfMissing,
                                            removeDirectory, removeFile,
                                            renameFile)
import           System.FilePath           (dropExtension, takeDirectory,
                                            takeExtension, takeFileName, (</>))
import           System.IO                 (IOMode (..), hFileSize, withFile)
import qualified Tox.Application.Tox       as Tox
import           Tox.AV                    (AudioFrame (AudioFrame),
                                            Picture (..))
import           Tox.Client                (initialClient)
import qualified Tox.Client.Avatars        as Avatars
import qualified Tox.Client.Debug          as Debug
import           Tox.Client.Export         (exportSave)
import qualified Tox.Client.Home           as Home
import           Tox.Client.Identicon      (identicon, identiconPng)
import           Tox.Client.Import         (ImportError (..), importSave,
                                            unlockSave)
import           Tox.Client.Locations      (location, mapsUri)
import qualified Tox.Client.Network        as Network
import           Tox.Client.Nodes          (builtIn)
import           Tox.Client.Pictures       (PictureFormat (..), pictureFormat,
                                            pictureFormats)
import           Tox.Client.Qoi            (isQoi, qoiBitmap, qoiSize)
import           Tox.Client.QrCode         (toxIdQr)
import           Tox.Client.Types          (entryText, shownText)
import qualified Tox.Client.Update         as Client
import qualified Tox.Core.Lanes            as Lanes
import           Tox.Core.Lanes            (Lanes)
import qualified Tox.Sync.Id               as Sync
import qualified Tox.Sync.Pairing          as Pairing

import           HTox.Links                (findLinks)
import           HTox.Model
import           HTox.Playback             (Output (..), pumpAudio)
import           HTox.Private              (private)
import qualified HTox.Store                as Store
import           HTox.Theme                (dp, theme)
import           HTox.View

-- | Update notifications for unread messages, missed calls and friend
-- requests, and the ringing call if this profile has it. Cancel what's been
-- read or answered.
notify :: Hub -> ProfileId -> Bool -> Model -> Model -> IO ()
notify hub pid owns old new = do
  let oldClient = modelClient old
      newClient = modelClient new
  now <- getCurrentTime
  named <- readIORef (hubNamed hub)
  let titled title
        | named = title <> " \x00B7 " <> profileNickname (clientProfile newClient)
        | otherwise = title
  let missed n = [if n == 1 then "Missed call" else Text.pack (show n) <> " missed calls"]
  forM_ [ (messagesKey, "Messages", "ic_chat_bubble_outline", MessageNotification, notifications now)
        , (missedKey, "Missed calls", "ic_call_missed", MissedCall, fmap missed . missedCalls now) ] $
    \(keyOf, channel, icon, kind, shown) -> do
      let before = shown oldClient
          after = shown newClient
      forM_ (Map.toList after) $ \(cid, lines') ->
        unless (Map.lookup cid before == Just lines') $
          forM_ (Map.lookup cid (clientContacts newClient)) $ \contact -> showNotification Notification
            { notificationKey = keyed pid (keyOf cid)
            , notificationChannel = channel
            , notificationIcon = icon
            , notificationTitle = titled (contactLabel cid contact)
            , notificationLines = lines'
            , notificationKind = kind
            }
      forM_ (Map.keys (Map.difference before after)) (cancelNotification . keyed pid . keyOf)
  forM_ (Map.toList (clientRequests newClient)) $ \(cid, message) ->
    unless (Map.lookup cid (clientRequests oldClient) == Just message) $ showNotification Notification
      { notificationKey = keyed pid (requestKey cid)
      , notificationChannel = "Friend requests"
      , notificationIcon = "ic_add"
      , notificationTitle = titled "Friend request"
      , notificationLines = [message]
      , notificationKind = MessageNotification
      }
  forM_ (Map.keys (Map.difference (clientRequests oldClient) (clientRequests newClient))) (cancelNotification . keyed pid . requestKey)
  let groupsBefore = groupNotifications now oldClient
      groupsAfter = groupNotifications now newClient
  forM_ (Map.toList groupsAfter) $ \(gid, lines') ->
    unless (Map.lookup gid groupsBefore == Just lines') $
      forM_ (Map.lookup gid (clientGroups newClient)) $ \g -> showNotification Notification
        { notificationKey = keyed pid (groupKey gid)
        , notificationChannel = "Messages"
        , notificationIcon = "ic_group"
        , notificationTitle = titled (groupLabel g)
        , notificationLines = lines'
        , notificationKind = MessageNotification
        }
  forM_ (Map.keys (Map.difference groupsBefore groupsAfter)) (cancelNotification . keyed pid . groupKey)
  let pending = Map.filter (isNothing . inviteAnswered) . clientInvites
  forM_ (Map.toList (pending newClient)) $ \(at@(cid, _), invite) ->
    unless (Map.member at (pending oldClient)) $ showNotification Notification
      { notificationKey = keyed pid (inviteKey at)
      , notificationChannel = "Group invites"
      , notificationIcon = "ic_group"
      , notificationTitle = titled (inviteLabel invite)
      , notificationLines = [maybe "A friend" (contactLabel cid) (Map.lookup cid (clientContacts newClient)) <> " invited you"]
      , notificationKind = MessageNotification
      }
  forM_ (Map.keys (Map.difference (pending oldClient) (pending newClient))) (cancelNotification . keyed pid . inviteKey)
  -- The service notification shows the call with its time.
  let ongoing client = case clientCall client of
        Just c | Talking since <- callPhase c ->
          (\contact -> (contactLabel (callContact c) contact, since)) <$> Map.lookup (callContact c) (clientContacts client)
        _ -> Nothing
  when (owns && ongoing oldClient /= ongoing newClient) $ setOngoingCall (keyed pid callNotification) (ongoing newClient)
  when (clientConnection oldClient /= clientConnection newClient) $ connected hub pid (Just (clientConnection newClient))
  when (settingStay (clientDevice oldClient) /= settingStay (clientDevice newClient)) $
    setRunInBackground (settingStay (clientDevice newClient))
  -- Only the call shows over the lock screen, never the chats.
  let overLockScreen m = isJust (clientCall (modelClient m)) && not (callHidden (modelPhoneCall m))
  when (owns && overLockScreen old /= overLockScreen new) $ setShowOverLockScreen (overLockScreen new)
  let inCall = isJust . clientCall
  when (owns && inCall oldClient /= inCall newClient) $ setKeepScreenOn (inCall newClient)
  when owns $ case (ringing old, ringing new) of
    (Nothing, Just c) -> forM_ (Map.lookup (callContact c) (clientContacts newClient)) $ \contact ->
      showNotification Notification
        { notificationKey = keyed pid callNotification
        , notificationChannel = "Incoming calls"
        , notificationIcon = "ic_call"
        , notificationTitle = titled (contactLabel (callContact c) contact)
        , notificationLines = [if callPeerVideo c then "Incoming video call" else "Incoming voice call"]
        , notificationKind = RingingCall
        }
    (Just _, Nothing) -> cancelNotification (keyed pid callNotification)
    _ -> pure ()
  where
    -- Only while hTox isn't in front (otherwise it rings itself, and nothing
    -- should cover the call screen).
    ringing model = case clientCall (modelClient model) of
      Just c | callPhase c == Ringing, not (modelShown model) -> Just c
      _                                                       -> Nothing

-- | Records a profile's connection to Tox ('Nothing' once the profile stops),
-- and shows the best of all profiles' in the service notification.
connected :: Hub -> ProfileId -> Maybe Connection -> IO ()
connected hub pid connection = modifyMVar_ (hubConnections hub) $ \known -> do
  let known' = Map.alter (const connection) pid known
      best = maximum . (Offline :) . Map.elems
  when (best known /= best known') $ setServiceStatus (connectionText (best known'))
  pure known'

-- | Longest side of pictures from the gallery or camera (we also strip the
-- location).
pictureSide :: Int
pictureSide = 2048

-- | How long we stay online after hTox is closed, with stay online off.
stayGrace :: NominalDiffTime
stayGrace = 15

debugSampleSeconds :: Int
debugSampleSeconds = 10


-- | Each profile's Tox and what its model holds, and the process's numbers
-- once.
debugSample :: Map.Map ProfileId Session -> IO ([(Text.Text, Int)], Map.Map Text.Text Debug.Report)
debugSample sessions = do
  measured <- forM (Map.toList sessions) $ \(ProfileId pid, s) -> do
    report <- either (const Nothing) Just <$> try @SomeException (Network.measure (sessionNetwork s))
    holds <- clientHolds . modelClient <$> readMVar (sessionModel s)
    pure (Text.pack ("profile " <> show pid), report, holds)
  let reports = [ (name, r) | (name, Just r, _) <- measured ]
      perProfile (name, report, holds) =
        maybe [] (Debug.toxRows name) report ++ [ (name <> " holds " <> k, v) | (k, v) <- holds ]
  pure (concatMap (Debug.processRows . snd) (take 1 reports) ++ concatMap perProfile measured, Map.fromList reports)


forkNamed :: String -> IO () -> IO ThreadId
forkNamed name act = forkIO ((myThreadId >>= (`labelThread` name)) >> act)

callNotification :: Text.Text
callNotification = "call"

pcmBytes :: V.Vector Int16 -> BS.ByteString
pcmBytes pcm = let (p, n) = V.unsafeToForeignPtr0 pcm in BS.fromForeignPtr0 (castForeignPtr p) (n * 2)

-- | Where Hatter plays call audio.
callOutput :: Output
callOutput = Output
  { outPlay = \(AudioFrame rate channels pcm) -> playCallAudio rate channels (pcmBytes pcm)
  , outLatency = callAudioLatency
  , outClock = getMonotonicTime
  , outSleep = threadDelay
  }

bytesPcm :: BS.ByteString -> V.Vector Int16
bytesPcm bytes = let (p, n) = BS.toForeignPtr0 bytes in V.unsafeFromForeignPtr0 (castForeignPtr p) (n `div` 2)

-- | We send audio to Tox in 20 ms frames.
audioRate, audioFrame :: Int
audioRate = 48000
audioFrame = 960

pictureFrame :: Picture -> VideoFrame
pictureFrame p = VideoFrame (pictureWidth p) (pictureHeight p) (BS.concat [pictureY p, pictureU p, pictureV p])

framePicture :: VideoFrame -> Picture
framePicture f = Picture w h y u v
  where
    w = frameWidth f
    h = frameHeight f
    (y, chroma) = BS.splitAt (w * h) (frameI420 f)
    (u, v) = BS.splitAt (half w * half h) chroma
    half n = (n + 1) `div` 2

mirrored :: VideoFrame -> VideoFrame
mirrored f = f { frameI420 = BS.concat (rows w y ++ rows half u ++ rows half v) }
  where
    w = frameWidth f
    half = (w + 1) `div` 2
    (y, chroma) = BS.splitAt (w * frameHeight f) (frameI420 f)
    (u, v) = BS.splitAt (BS.length chroma `div` 2) chroma
    rows n plane
      | BS.null plane = []
      | otherwise = let (row, rest) = BS.splitAt n plane in BS.reverse row : rows n rest

-- | What a picked file is for. Pictures may get converted to the chat's
-- picture format.
data Picking = ForPicture | ForFile | ForProfile | ForBackup
  deriving (Eq)

data Media = Media
  { mediaAudio   :: IORef (Maybe CallAudio)
  , mediaCamera  :: IORef (Maybe (CallCamera, ThreadId))
    -- ^ Filming, and sending what it films.
  , mediaMuted   :: IORef Bool
  , mediaPlayers :: IORef [ThreadId] -- ^ Playing the call's audio and showing its video.
  }

callMedia :: Lanes -> Network.Network -> Media -> (Route -> [Route] -> IO ()) -> Model -> Model -> IO ()
callMedia lanes network media onRoutes old new = do
  case (talking old, talking new) of
    (Nothing, Just cid) -> do
      let speaker = callRoute (modelPhoneCall new) == Speaker
          sending frame = do
            silent <- readIORef (mediaMuted media)
            unless silent $ Network.sendAudio network cid audioRate 1 (bytesPcm frame)
      started <- startCallAudio audioRate audioFrame speaker sending $ \route routes ->
        onRoutes (fromCallRoute route) (map fromCallRoute routes)
      writeIORef (mediaAudio media) (Just started)
      players <- sequence
        [ forkNamed "htox call audio" (pumpAudio lanes callOutput (Network.pullAudio network cid))
        , forkNamed "htox call video" (showPictures lanes network cid)
        ]
      writeIORef (mediaPlayers media) players
    (Just _, Nothing) -> do
      readIORef (mediaPlayers media) >>= mapM_ killThread
      writeIORef (mediaPlayers media) []
      stopping (mediaAudio media) stopCallAudio
    _ -> pure ()
  -- Restart with the other camera when the user flips it.
  when (filming old /= filming new) $ do
    stopping (mediaCamera media) (\(camera, sender) -> stopCallCamera camera >> killThread sender)
    forM_ (filming new) $ \(cid, backCamera) -> do
      -- The latest frame waits for its own thread to send it, so the camera
      -- never waits for the encoder, and frames it films meanwhile are
      -- left out.
      latest <- newEmptyMVar
      sender <- forkNamed "htox call camera" . forever $ takeMVar latest >>= Network.sendVideo network cid . framePicture
      started <- startCallCamera (not backCamera) $ \frame -> do
        -- Mirror our own view (the friend gets it unmirrored).
        showVideoFrame selfStream (if backCamera then frame else mirrored frame)
        tryTakeMVar latest >> void (tryPutMVar latest frame)
      writeIORef (mediaCamera media) (Just (started, sender))
  writeIORef (mediaMuted media) (maybe False callMuted (clientCall (modelClient new)))
  let tone model = case clientCall (modelClient model) of
        Just c | isJust (callDevice c)                 -> Quiet
               | callPhase c == Ringing, modelShown model -> Ringtone
               | callPhase c == Calling                -> Ringback
        _                                              -> Quiet
  when (tone old /= tone new) $ setCallTone (tone new)
  where
    -- Not one a linked device took: we only relay that.
    talking model = case clientCall (modelClient model) of
      Just c | Talking _ <- callPhase c, isNothing (callDevice c) -> Just (callTarget c)
      _ -> Nothing
    filming model = case clientCall (modelClient model) of
      Just c | callCamera c -> (, callBackCamera (modelPhoneCall model)) <$> talking model
      _                     -> Nothing
    stopping ref stop = readIORef ref >>= mapM_ stop >> writeIORef ref Nothing

-- | Every 16 ms, about a screen refresh, shows the call's picture that's due
-- by the time the next one shows.
showPictures :: Lanes -> Network.Network -> ContactId -> IO ()
showPictures lanes network cid = forever $ do
  threadDelay 16000
  Network.dueVideo network cid 33 >>= mapM_ (Lanes.timed lanes "Call video display" . showVideoFrame peerStream . pictureFrame)

fromCallRoute :: CallRoute -> Route
fromCallRoute = \case
  CallAudio.Earpiece  -> Earpiece
  CallAudio.Speaker   -> Speaker
  CallAudio.Wired     -> Wired
  CallAudio.Bluetooth -> Bluetooth

toCallRoute :: Route -> CallRoute
toCallRoute = \case
  Earpiece  -> CallAudio.Earpiece
  Speaker   -> CallAudio.Speaker
  Wired     -> CallAudio.Wired
  Bluetooth -> CallAudio.Bluetooth

messagesKey, missedKey, requestKey :: ContactId -> Text.Text
messagesKey (ContactId key) = key
missedKey (ContactId key) = "missed-" <> key
requestKey (ContactId key) = "request-" <> key

groupKey :: GroupId -> Text.Text
groupKey (GroupId chat) = "group-" <> chat

inviteKey :: (ContactId, GroupId) -> Text.Text
inviteKey (ContactId friend, GroupId chat) = "invite-" <> friend <> "-" <> chat

keyed :: ProfileId -> Text.Text -> Text.Text
keyed (ProfileId i) key = Text.pack (show i) <> ":" <> key

-- | Old ones (from before several profiles were online at once) have no
-- profile.
profileOf :: Text.Text -> (Maybe ProfileId, Text.Text)
profileOf key = case Text.breakOn ":" key of
  (i, rest) | not (Text.null rest), [(n, "")] <- reads (Text.unpack i) -> (Just (ProfileId n), Text.drop 1 rest)
  _ -> (Nothing, key)

notificationOf :: Text.Text -> Maybe Screen
notificationOf key
  | Just k <- Text.stripPrefix "missed-" key = Just (Chat (ContactId k))
  | Just _ <- Text.stripPrefix "request-" key = Just Contacts
  | Just k <- Text.stripPrefix "group-" key = Just (GroupChat (GroupId k))
  | Just k <- Text.stripPrefix "invite-" key, (friend, chat) <- Text.breakOn "-" k =
      Just (InviteScreen (ContactId friend) (GroupId (Text.drop 1 chat)))
  | key == callNotification = Nothing
  | otherwise = Just (Chat (ContactId key))

voices :: [(Text.Text, Tox.Voice)]
voices = [("Everyone but observers", Tox.VoiceAll), ("Moderators", Tox.VoiceModerators), ("The founder", Tox.VoiceFounder)]

memberActionName :: MemberAction -> Text.Text
memberActionName = \case
  MakeModerator -> "Make moderator"
  MakeUser      -> "Make a member who can speak"
  MakeObserver  -> "Make observer"
  Kick          -> "Remove from the group"

-- | Choices of the + button.
adding :: [(Text.Text, Screen)]
adding = [("Contact", AddContact), ("Group", NewGroupScreen), ("Public group by its chat ID", JoinGroupScreen)]

autoAccepts :: [(Text.Text, Word64)]
autoAccepts =
  ("Never", 0) : [(Text.pack (show n) <> " MB", n * 1024 * 1024) | n <- [1, 4, 16, 32, 64, 256]]

routeName :: Route -> Text.Text
routeName = \case
  Earpiece  -> "Phone"
  Speaker   -> "Speaker"
  Wired     -> "Headset"
  Bluetooth -> "Bluetooth"

muteChoices :: [(Text.Text, Maybe NominalDiffTime)]
muteChoices =
  [("For 1 hour", Just 3600), ("For 8 hours", Just 28800), ("For 1 week", Just 604800), ("Always", Nothing)]

toMillis :: UTCTime -> Integer
toMillis = round . (* 1000) . utcTimeToPOSIXSeconds

data ContactActions = ContactActions
  { caOpen      :: Action
  , caAccept    :: Action
  , caReject    :: Action
  , caDetails   :: Action
  , caMute      :: Action
  , caDelete    :: Action
  , caCopyKey   :: Action
  , caForwardTo :: Action
  , caCallBack  :: Action
  , caPictures  :: Action
  , caHold      :: Action
    -- ^ The chat list's menu of the chat.
  }

data GroupActions = GroupActions
  { gaOpen      :: Action
  , gaDetails   :: Action
  , gaMute      :: Action
  , gaLeave     :: Action
  , gaDelete    :: Action
  , gaCopyId    :: Action
  , gaTopic     :: Action
  , gaMentions  :: Action
  , gaInvite    :: Action
  , gaSettings  :: Action
  , gaPrivacy   :: Action
  , gaTopicLock :: Action
  , gaVoice     :: Action
  , gaRelayBot  :: Action
  , gaStopRelay :: Action
  , gaPictures  :: Action
  , gaHold      :: Action
    -- ^ As 'caHold'.
  }

data InviteActions = InviteActions
  { iaOpen    :: Action
  , iaJoin    :: Action
  , iaDecline :: Action
  }

newtype ToxFailed = ToxFailed Tox.NewError
  deriving (Show)

instance Exception ToxFailed where
  displayException (ToxFailed err) = "Tox didn't start: " <> show err

-- | All profiles online, each with its own Tox.
data Hub = Hub
  { hubDatabase    :: Store.Database
  , hubDir         :: FilePath
  , hubSessions    :: MVar (Map.Map ProfileId Session)
  , hubShown       :: IORef ProfileId
  , hubRedraw      :: IORef (IO ())
  , hubMedia       :: Media
  , hubCall        :: IORef (Maybe (ProfileId, Call))
    -- ^ Profile with the call, and the call. There's at most 1 (we have 1
    -- microphone and speaker).
  , hubConnections :: MVar (Map.Map ProfileId Connection)
  , hubNamed       :: IORef Bool
    -- ^ Name the profile in notifications (more than 1 is online).
  , hubWatch       :: ThreadId
  , hubWriting     :: ThreadId
    -- ^ Writes what the profiles save, so their events don't wait for it.
  , hubLanes       :: Lanes
    -- ^ Time hTox spends on each kind of work, in all profiles.
  , hubSummaries   :: IORef (Map.Map ProfileId ProfileSummary)
    -- ^ Of each profile online, for the others to show without waiting for
    -- its model.
  }

data Session = Session
  { sessionModel   :: MVar Model
  , sessionNetwork :: Network.Network
  , sessionFire    :: Event -> IO ()
  , sessionStop    :: IO ()
  }

-- | Puts every profile online. It fails if the shown one can't go online,
-- while others that can't stay offline.
newHub :: IORef (IO ()) -> IO Hub
newHub redraw = do
  dir <- getAppFilesDir
  key <- databaseKey >>= \case
    DatabaseKey key -> pure (Just key)
    NoKeyStore      -> pure Nothing
    KeyStoreFailed  -> throwIO Store.Locked
  database <- Store.open key (dir </> "tox.db")
  mapM_ (createDirectoryIfMissing True) [dir </> "received", dir </> "sent"]
  shown <- Store.activeProfile database
  sessions <- newMVar Map.empty
  media <- Media <$> newIORef Nothing <*> newIORef Nothing <*> newIORef False <*> newIORef []
  lanes <- Lanes.newLanes
  writing <- forkNamed "htox writing" (Store.writing platformLog (Lanes.timed lanes "Writing") database)
  -- With stay online off, stop a while after hTox is closed (not during a
  -- call).
  watchThread <- forkNamed "htox stay online" $ do
    let watch since = do
          threadDelay 5000000
          models <- mapM (readMVar . sessionModel) . Map.elems =<< readMVar sessions
          now <- getCurrentTime
          if any (\m -> settingStay (clientDevice (modelClient m)) || modelShown m || isJust (clientCall (modelClient m))) models
            then watch now
            else if diffUTCTime now since >= stayGrace then stopBackground else watch since
    getCurrentTime >>= watch
  hub <- Hub database dir sessions
    <$> newIORef shown <*> pure redraw
    <*> pure media <*> newIORef Nothing <*> newMVar Map.empty <*> newIORef False <*> pure watchThread
    <*> pure writing <*> pure lanes <*> newIORef Map.empty
  -- Where time and memory went, kept in files for the debug export, and once
  -- a minute in the log for whoever reads it.
  logged <- newIORef Map.empty
  void . forkNamed "htox debug" . forM_ [1 :: Int ..] $ \n -> do
    threadDelay (debugSampleSeconds * 1000000)
    (rows, reports) <- debugSample =<< readMVar sessions
    now <- getCurrentTime
    try @IOException (Debug.recordSample dir now rows) >>= either (platformLog . Text.pack . displayException) pure
    when (n `mod` (60 `div` debugSampleSeconds) == 0) $ do
      before <- readIORef logged
      forM_ (Map.toList reports) $ \(name, report) ->
        platformLog ("Debug: " <> name <> ": " <> Debug.summary (Map.lookup name before) report)
      writeIORef logged reports
  setServiceStatus (connectionText Offline)
  -- If the shown profile can't go online, stop the watch too, or it stops
  -- the app while the user is about to try again.
  void (sessionFor hub shown) `onException` mapM_ killThread [watchThread, writing]
  others <- filter (/= shown) . map accountId <$> Store.profiles database
  forM_ others $ \pid -> try @SomeException (sessionFor hub pid) >>= \case
    Left err -> platformLog ("A profile didn't go online: " <> Text.pack (displayException err))
    Right _ -> pure ()
  setRunInBackground . settingStay . clientDevice . modelClient =<< readMVar . sessionModel =<< sessionFor hub shown
  pure hub

stopHub :: Hub -> IO ()
stopHub hub = do
  killThread (hubWatch hub)
  modifyMVar_ (hubSessions hub) $ \sessions -> Map.empty <$ mapM_ sessionStop sessions
  killThread (hubWriting hub)
  Store.flush (hubDatabase hub)

-- | Goes online first if it isn't.
sessionFor :: Hub -> ProfileId -> IO Session
sessionFor hub pid = modifyMVar (hubSessions hub) $ \sessions -> case Map.lookup pid sessions of
  Just session -> pure (sessions, session)
  Nothing -> do
    session <- online hub pid
    let sessions' = Map.insert pid session sessions
    writeIORef (hubNamed hub) (Map.size sessions' > 1)
    pure (sessions', session)

-- | Also cancels its notifications.
goOffline :: Hub -> ProfileId -> IO ()
goOffline hub pid = modifyMVar_ (hubSessions hub) $ \sessions -> do
  mapM_ sessionStop (Map.lookup pid sessions)
  atomicModifyIORef' (hubSummaries hub) (\known -> (Map.delete pid known, ()))
  let sessions' = Map.delete pid sessions
  writeIORef (hubNamed hub) (Map.size sessions' > 1)
  pure sessions'

redrawIfShown :: Hub -> ProfileId -> IO ()
redrawIfShown hub pid = readIORef (hubShown hub) >>= \shown -> when (shown == pid) (join (readIORef (hubRedraw hub)))

online :: Hub -> ProfileId -> IO Session
online hub pid = do
  let store = Store.profileStore (hubDatabase hub) pid
      redraw = redrawIfShown hub pid
      lanes = hubLanes hub
  model <- newMVar =<< Store.load store
  void (summarised hub pid model)
  identity <- Store.identity store
  settings <- clientDevice . modelClient <$> readMVar model
  -- Cancel a call that was ringing when hTox last stopped.
  cancelNotification (keyed pid callNotification)
  network <- either (throwIO . ToxFailed) pure =<< Network.start platformLog lanes identity settings (hubDir hub </> "received") builtIn
  unless (isJust identity) $ Network.identityOf network >>= Store.saveIdentity store
  -- Run Tox commands in order on their own thread (Tox may be slow, and a
  -- tap shouldn't wait for it).
  asked <- newChan
  let fire event = do
        (commands, busy, device, owns) <- Lanes.timed lanes ("Event " <> Network.constructor event) . modifyMVar model $ \old -> do
          let (new, commands) = update event old
          Store.save store (modelClient old) (modelClient new)
          owns <- claimCall hub pid old new
          notify hub pid owns old new
          -- Fork (the platform reports routes while we hold the model).
          when owns $ callMedia lanes network (hubMedia hub)
            (\route routes -> void . forkNamed "htox route change" $ fire (RoutesChanged route routes) >> redraw)
            old new
          let was = modelClient old
              is = modelClient new
              busy = not owns && isNothing (clientCall was) && isJust (clientCall is)
          pure (new, (commands, busy, if clientDevice was /= clientDevice is then Just (clientDevice is) else Nothing, owns))
        writeList2Chan asked commands
        changed <- summarised hub pid model
        -- Another profile shown has a banner for our call, and us in its
        -- drawer.
        shown <- readIORef (hubShown hub)
        when ((owns || changed) && shown /= pid) $ join (readIORef (hubRedraw hub))
        when busy $ getCurrentTime >>= fire . ClientEvent . Client.CallElsewhere
        forM_ device $ \d -> do
          others <- Map.elems . Map.delete pid <$> readMVar (hubSessions hub)
          forM_ others (`sessionFire` ClientEvent (Client.SetDevice d))
  -- Each command runs once what it changed is written, as Tox can't take it
  -- back.
  askingThread <- Network.looping network "Asking Tox" $
    readChan asked >>= (\c -> Store.written (hubDatabase hub) >> Lanes.timed lanes ("Command " <> Network.constructor c) (Network.perform network c))
      >>= mapM_ (fire . ClientEvent) >> redraw
  networkThreads <- Network.run network (\event -> Store.recalled store event >>= fire . ClientEvent >> redraw)
  -- For the call timer and ring timeout, to say we stopped typing, and to
  -- measure again on the debug screen.
  tickThread <- forkNamed "htox ticker" . forever $ do
    threadDelay 1000000
    ticking <- (\m -> isJust (clientCall (modelClient m)) || not (Map.null (clientTypingTo (modelClient m)))
                     || modelScreen m == DebugScreen) <$> readMVar model
    when ticking $ getCurrentTime >>= fire . ClientEvent . Client.Tick >> redraw
  let stop = modifyMVar_ model $ \current -> do
        mapM_ killThread (askingThread : tickThread : networkThreads)
        Network.stop network
        -- Cancel its notifications.
        owns <- claimCall hub pid current initial
        let client = modelClient current
        notify hub pid owns current initial
          { modelClient = initialClient { clientDevice = clientDevice client, clientProfile = clientProfile client } }
        connected hub pid Nothing
        pure current
  pure (Session model network fire stop)

-- | Whether its summary changed.
summarised :: Hub -> ProfileId -> MVar Model -> IO Bool
summarised hub pid model = do
  now <- getCurrentTime
  summary <- profileSummary now . modelClient <$> readMVar model
  atomicModifyIORef' (hubSummaries hub) $ \known ->
    (Map.insert pid summary known, Map.lookup pid known /= Just summary)

-- | Whether the profile has the call. It takes it if no other profile has
-- one, and gives it up when its call ends.
claimCall :: Hub -> ProfileId -> Model -> Model -> IO Bool
claimCall hub pid old new = atomicModifyIORef' (hubCall hub) $ \owner -> case (clientCall (modelClient old), clientCall (modelClient new)) of
  (_, Just c) | maybe True ((== pid) . fst) owner -> (Just (pid, c), True)
  (Just _, Nothing) | (fst <$> owner) == Just pid -> (Nothing, True)
  _                                               -> (owner, False)

-- | The app, or why hTox didn't start, with a button to try again.
app :: IO MobileApp
app = do
  actions <- newActionState
  -- Running app, and how to stop it to show another profile.
  started <- newIORef Nothing
  failure <- newIORef Text.empty
  redraw <- newIORef (pure ())
  hub <- newIORef Nothing
  let start = try (hubbed >>= \h -> running actions h switch restart) >>= \case
        Right a -> writeIORef started (Just a)
        Left err@(SomeException inner) -> do
          platformLog ("hTox didn't start: " <> Text.pack (displayException err))
          -- Without the backtrace (that's in the log).
          writeIORef failure (Text.pack (displayException inner))
      hubbed = readIORef hub >>= \case
        Just h -> pure h
        Nothing -> newHub redraw >>= \h -> h <$ writeIORef hub (Just h)
      -- Show the newly chosen profile. All stay online.
      switch = do
        readIORef started >>= mapM_ snd
        writeIORef started Nothing
        start
        inApp (`onLifecycle` Start)
        join (readIORef redraw)
      -- Restart all profiles (Tox takes some settings only at the start).
      restart = do
        readIORef hub >>= mapM_ stopHub
        writeIORef hub Nothing
        switch
      inApp f = readIORef started >>= mapM_ (f . maContext . fst)
  start
  -- The app is in front when this is pressed, so tell the new app.
  retry <- runActionM actions (createAction (start >> inApp (`onLifecycle` Start)))
  device <- getDeviceInfo
  pure MobileApp
    { maContext = loggingMobileContext
        { onLifecycle = \event -> inApp (`onLifecycle` event)
        , onBack = readIORef started >>= maybe (pure False) (onBack . maContext . fst)
        , onNotificationOpened = \key -> inApp (`onNotificationOpened` key)
        , onCallButton = \key button -> inApp (\c -> onCallButton c key button)
        , onFilePicked = \picked -> inApp (`onFilePicked` picked)
        , onPicturePicked = \png -> inApp (`onPicturePicked` png)
        , onRendered = \ns -> readIORef hub >>= mapM_ (\h -> Lanes.took (hubLanes h) "Render" ns)
        }
    , maActionState = actions
    , maView = \state -> writeIORef redraw (userRequestRedraw state) >> readIORef started >>= \case
        Just (a, _) -> maView a state
        Nothing -> do
          dark <- isDarkMode
          startFailure (theme (diScreenDensity device) (fromIntegral (diScreenWidth device)) dark) retry
            <$> readIORef failure
    }

-- | App showing a profile, and how to stop it.
running :: ActionState -> Hub -> IO () -> IO () -> IO (MobileApp, IO ())
running actions hub switch restart = do
  let database = hubDatabase hub
      dir = hubDir hub
      sent = dir </> "sent"
      redraw = hubRedraw hub
  shownId <- Store.activeProfile database
  session <- sessionFor hub shownId
  writeIORef (hubShown hub) shownId
  let model = sessionModel session
      network = sessionNetwork session
      fire = sessionFire session
  -- Profiles may have changed while it wasn't shown.
  Store.profiles database >>= fire . ProfilesChanged
  permissions <- newIORef (Nothing :: Maybe PermissionState)
  user <- newIORef (Nothing :: Maybe UserState)
  locating <- newIORef False
  picking <- newIORef ForFile
  -- A .tox file waiting for its password.
  locked <- newIORef (Nothing :: Maybe BS.ByteString)
  let stop = fire (Shown False)
      -- A ringing call can't hide, so we stay with it.
      switching next = readMVar model >>= \current -> case callPhase <$> clientCall (modelClient current) of
        Just Ringing -> fire (Notify "Not while a call rings") >> join (readIORef redraw)
        _ -> next >>= \pid -> if pid == shownId then join (readIORef redraw)
          else fire Leave >> Store.setActive database pid >> switch
  let withUser f = readIORef user >>= mapM_ f
      redraw' = join (readIORef redraw)
      -- Import a .tox file as a profile, online at once, unless we have it
      -- already.
      taking bytes = opening bytes (importSave bytes)
      opening bytes = \case
        Left Encrypted -> writeIORef locked (Just bytes) >> fire (Open UnlockScreen) >> redraw'
        Left WrongPassword -> fire (Notify "Wrong password") >> redraw'
        Left NotAProfile -> fire (Notify "That isn't a Tox profile") >> redraw'
        Right (known, imported) -> do
          writeIORef locked Nothing
          others <- mapM (Store.identity . Store.profileStore database . accountId) =<< Store.profiles database
          if identitySecretKey known `elem` map identitySecretKey (catMaybes others)
            then fire (Notify "hTox has that profile already") >> redraw'
            else switching $ do
              pid <- Store.newProfile database (Just known) (clientProfile imported)
              Store.save (Store.profileStore database pid) initialClient imported
              pure pid
      shareLocation = withUser $ \u -> requestPermission (userPermissionState u) PermissionLocation $ \case
        PermissionDenied -> fire (Notify "hTox may not use your location") >> redraw'
        PermissionGranted -> do
          fire (Notify "Finding where you are\x2026") >> redraw'
          writeIORef locating True
          startLocationUpdates (userLocationState u) $ \fix -> do
            waiting <- atomicModifyIORef' locating (False,)
            when waiting $ do
              stopLocationUpdates (userLocationState u)
              fire (ShareLocation (ldLatitude fix) (ldLongitude fix)) >> redraw'
          void . forkNamed "htox location timeout" $ do
            threadDelay 30000000
            waiting <- atomicModifyIORef' locating (False,)
            when waiting $ do
              stopLocationUpdates (userLocationState u)
              fire (Notify "Couldn't find where you are") >> redraw'
      takePicture = withUser $ \u -> requestPermission (userPermissionState u) PermissionCamera $ \case
        PermissionDenied -> fire (Notify "hTox may not use the camera") >> redraw'
        PermissionGranted -> capturePicture
      attaching = withUser $ \u -> readMVar model >>= \current -> do
        -- Groups only take files.
        let choices = filter (\(name, _) -> name /= "Location" || not (inGroup current)) everything
        showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Attach" (map fst choices)) $ \case
          BottomSheetItemSelected n | (_, chosen) : _ <- drop (fromIntegral n) choices -> chosen
          _ -> pure ()
        where
          everything =
            [ ("Gallery", writeIORef picking ForPicture >> pickFiles "image/*")
            , ("Camera", writeIORef picking ForPicture >> takePicture)
            , ("File", writeIORef picking ForFile >> pickFiles "*/*"), ("Location", shareLocation) ]
          inGroup current = case modelScreen current of
            GroupChat _ -> True
            _           -> False
  -- No call without the microphone, and no video without the camera. We
  -- redraw after the answer (Hatter doesn't).
  let allowing camera next = readIORef permissions >>= \case
        Nothing -> next camera
        Just state -> requestPermission state PermissionMicrophone $ \case
          PermissionDenied -> fire (Notify "Calls need the microphone") >> join (readIORef redraw)
          PermissionGranted
            | camera -> requestPermission state PermissionCamera $ \case
                PermissionGranted -> next True >> join (readIORef redraw)
                PermissionDenied -> do
                  fire (Notify "hTox may not use the camera, so the call goes on without it")
                  next False >> join (readIORef redraw)
            | otherwise -> next False >> join (readIORef redraw)
  handlers <- runActionM actions $ do
    back' <- createAction (fire Back)
    send <- createAction (getCurrentTime >>= fire . Send)
    openDrawer <- createAction (fire OpenDrawer)
    openProfile <- createAction (fire (Open ProfileScreen))
    openAddContact <- createAction $ withUser $ \u ->
      showBottomSheet (userBottomSheetState u) (BottomSheetConfig "New" (map fst adding)) $ \case
        BottomSheetItemSelected n | (_, screen') : _ <- drop (fromIntegral n) adding -> fire (Open screen') >> redraw'
        _ -> pure ()
    openChats <- createAction (fire (Open Contacts))
    openCalls <- createAction (fire (Open CallsScreen))
    allowFullScreen' <- createAction allowFullScreen
    applyProfile <- createAction (fire ApplyProfile)
    addFriend <- createAction (fire AddFriend)
    copyToxId <- createAction $
      readMVar model >>= copyToClipboard . profileToxId . clientProfile . modelClient
    attach <- createAction attaching
    openSettings <- createAction (fire (Open SettingsScreen))
    openProfiles <- createAction (fire (Open ProfilesScreen))
    newProfile <- createAction (switching (Store.newProfile database Nothing (clientProfile initialClient)))
    importProfile <- createAction (writeIORef picking ForProfile >> pickFiles "*/*")
    openBackup <- createAction (fire (Open BackupScreen))
    openDebug <- createAction (fire (Open DebugScreen))
    restoreBackup <- createAction (writeIORef picking ForBackup >> pickFiles "*/*")
    -- Written to our files, then copied where the user chooses.
    backUp <- createAction $ do
      chosen <- Set.toList . modelBackup <$> readMVar model
      let path = dir </> "backup.sqlite"
      try @SomeException (Store.backUp database chosen path) >>= \case
        Left err -> do
          platformLog ("No backup: " <> Text.pack (displayException err))
          void (try @IOException (removeFile path))
          fire (Notify "Couldn't back up the profiles") >> redraw'
        Right () -> do
          today <- utctDay <$> getCurrentTime
          saveFile path ("htox-" <> Text.pack (show today) <> ".backup") "application/octet-stream"
    -- Try these sizes until the PNG is small enough for Tox clients.
    pickAvatar <- createAction (pickPicture [256, 192, 128, 96, 64] Avatars.maxSize)
    removeAvatar <- createAction (fire (ClientEvent Client.RemoveAvatar))
    -- The last 30 minutes of debug samples, copied where the user chooses.
    exportDebug <- createAction $ do
      now <- getCurrentTime
      (rows, _) <- debugSample =<< readMVar (hubSessions hub)
      heap <- Debug.heapByType (dir </> "heap.hp")
      let path = dir </> "debug.tsv"
      Text.writeFile path =<< Debug.export dir now (rows ++ heap)
      saveFile path ("htox-debug-" <> Text.pack (formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now) <> ".tsv") "text/tab-separated-values"
    exportProfile <- createAction $ do
      current <- readMVar model
      me <- Network.identityOf network
      forM_ (exportSave me (modelClient current)) $ \save -> do
        let path = dir </> "export.tox"
        private path
        BS.writeFile path save
        saveFile path (profileNickname (clientProfile (modelClient current)) <> ".tox") "application/octet-stream"
    chooseAway <- createAction $ withUser $ \u ->
      showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Status" (map awayName aways)) $ \case
        BottomSheetItemSelected n | away : _ <- drop (fromIntegral n) aways -> fire (ClientEvent (Client.SetAway away)) >> redraw'
        _ -> pure ()
    chooseAutoAccept <- createAction $ withUser $ \u ->
      showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Accept files without asking" (map fst autoAccepts)) $ \case
        BottomSheetItemSelected n | (_, size) : _ <- drop (fromIntegral n) autoAccepts -> fire (ClientEvent (Client.SetAutoAccept size)) >> redraw'
        _ -> pure ()
    let toggling f = createAction (readMVar model >>= fire . ClientEvent . Client.SetDevice . f . clientDevice . modelClient)
    toggleUdp <- toggling (\d -> d { settingUdp = not (settingUdp d) })
    toggleIpv6 <- toggling (\d -> d { settingIpv6 = not (settingIpv6 d) })
    toggleLan <- toggling (\d -> d { settingLan = not (settingLan d) })
    toggleStay <- toggling (\d -> d { settingStay = not (settingStay d) })
    toggleTyping <- createAction $
      readMVar model >>= fire . ClientEvent . Client.SetTypingIndicators . not . settingTyping . clientAccount . modelClient
    openProxy <- createAction (fire (Open ProxyScreen))
    proxyKinds <- (,,,)
      <$> createAction (fire (ProxyKindChosen Nothing))
      <*> createAction (fire (ProxyKindChosen (Just Tox.Socks5Proxy)))
      <*> createAction (fire (ProxyKindChosen (Just Tox.HttpProxy)))
      <*> createAction (fire (ProxyKindChosen (Just Tox.GatewayProxy)))
    proxyHost <- createOnChange (fire . ProxyHostChanged)
    proxyPort <- createOnChange (fire . ProxyPortChanged)
    -- Tox only takes a proxy at the start, so restart (not during a call).
    applyProxy <- createAction $ do
      before <- settingProxy . clientDevice . modelClient <$> readMVar model
      fire ApplyProxy
      current <- readMVar model
      if settingProxy (clientDevice (modelClient current)) == before || isJust (clientCall (modelClient current))
        then redraw'
        else restart
    draft <- createOnChange (\text -> getCurrentTime >>= fire . DraftChanged text)
    nickname <- createOnChange (fire . NicknameChanged)
    status <- createOnChange (fire . StatusChanged)
    address <- createOnChange (fire . AddressChanged)
    request <- createOnChange (fire . RequestChanged)
    let twice f = (,) <$> createAction (f False) <*> createAction (f True)
    calls <- twice $ \camera -> allowing camera (\allowed -> getCurrentTime >>= \now -> fire (StartCall now allowed))
    answers <- twice $ \camera -> allowing camera (\allowed -> getCurrentTime >>= \now -> fire (ClientEvent (Client.AnswerCall now allowed)))
    hangUp <- createAction (getCurrentTime >>= fire . ClientEvent . Client.HangUp)
    mute <- createAction (fire (ClientEvent Client.ToggleMute))
    camera' <- createAction (fire (ClientEvent Client.ToggleCamera))
    -- The platform tells us the new route.
    route <- createAction $ readMVar model >>= (\c -> case callRoutes c of
      routes | length routes <= 2 -> setCallRoute (toCallRoute (if callRoute c == Speaker then Earpiece else Speaker))
             | otherwise -> withUser $ \u ->
                 showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Audio" (map routeName routes)) $ \case
                   BottomSheetItemSelected n | r : _ <- drop (fromIntegral n) routes -> setCallRoute (toCallRoute r)
                   _ -> pure ()) . modelPhoneCall
    flipCamera <- createAction (fire FlipCamera)
    hideCall <- createAction (fire HideCall)
    showCall <- createAction (fire ShowCall)
    returnToCall <- createAction $ readIORef (hubCall hub) >>= mapM_ (\(pid, _) -> do
      sessions <- readMVar (hubSessions hub)
      forM_ (Map.lookup pid sessions) (`sessionFire` ShowCall)
      switching (pure pid))
    -- At the start of what the open chat has, the page before it.
    scrolled <- createOnScroll $ \position -> do
      fire (ScrolledAway (position /= ListEnd))
      when (position == ListStart) $ readMVar model >>= mapM_ (\(chat, oldest) ->
        Store.older (Store.profileStore database shownId) chat oldest >>= fire . ClientEvent) . pageBefore
    jump <- createAction (fire JumpToEnd)
    unselect <- createAction (fire Unselect)
    let selected f = readMVar model >>= \current ->
          forM_ (openChat current) $ \chat -> f current chat (Set.toList (modelSelected current))
    copySelected <- createAction $ selected $ \current chat is ->
      copyToClipboard (copiedText (modelClient current) chat is) >> fire Unselect
    forwardSelected <- createAction $ selected $ \_ chat is -> fire (Open (Forward chat is))
    cancelReply <- createAction (fire CancelReply)
    password <- createOnChange (fire . PasswordChanged)
    topic <- createOnChange (fire . TopicEdited)
    applyTopic <- createAction (fire ApplyTopic)
    foundings <- Map.fromList <$> traverse (\f -> (,) f <$> createAction (fire (FoundingChosen f))) [minBound .. maxBound]
    groupName' <- createOnChange (fire . GroupNameEdited)
    found <- createAction (fire Found)
    chatId <- createOnChange (fire . ChatIdEdited)
    joinPublic <- createAction (fire JoinPublic)
    endPrivate <- createAction (fire (PrivatelyTo Nothing))
    peerLimit <- createOnChange (fire . PeerLimitEdited)
    groupPassword <- createOnChange (fire . GroupPasswordEdited)
    applyGroupSettings <- createAction (fire ApplyGroupSettings)
    openLink <- createAction (fire (Open LinkScreen))
    openDevices <- createAction (fire (Open DevicesScreen))
    -- A new code, which stops working after a while.
    pairDevice <- createAction $ do
      code <- Text.decodeUtf8 . Base16.encode <$> randomByteString 16
      now <- getCurrentTime
      fire (ClientEvent (Client.Pair code now))
      void . forkNamed "htox pairing code" $ do
        threadDelay (floor (Home.pairingTime * 1000000))
        fire (ClientEvent (Client.Unpair code)) >> redraw'
    copyPairing <- createAction (readMVar model >>= mapM_ copyToClipboard . pairingLink)
    linkEdited <- createOnChange (fire . LinkEdited)
    -- Deriving the key takes a moment.
    unlock <- createAction $ readMVar model >>= \current -> readIORef locked >>= mapM_ (\bytes ->
      void . forkNamed "htox profile unlocking" $
        opening bytes =<< evaluate (importSave =<< unlockSave (modelPassword current) bytes))
    -- New profile for the linked account, online at once.
    link <- createAction $ readMVar model >>= \current ->
      case (linkError (modelLink current), Pairing.parseLink (modelLink current)) of
        (Nothing, Just pairing) -> do
          now <- getCurrentTime
          switching (Store.newLinkedProfile database (Pairing.linkAddress pairing) (Pairing.requestMessage pairing) now)
        (why, _) -> fire (Notify (fromMaybe "That isn't a pairing link" why)) >> redraw'
    pure $ \perContact perMessage perEntry perProfile perGroup perInvite perInviting perMember perStaged perGroupFile perDevice -> Handlers
      { hBack = back'
      , hSend = send
      , hOpenDrawer = openDrawer
      , hOpenProfile = openProfile
      , hOpenAddContact = openAddContact
      , hOpenChats = openChats
      , hOpenCalls = openCalls
      , hCallBack = caCallBack . perContact
      , hAllowFullScreen = allowFullScreen'
      , hApplyProfile = applyProfile
      , hAddFriend = addFriend
      , hCopyToxId = copyToxId
      , hAttach = attach
      , hTapMessage = \cid i -> let (tap, _, _, _) = perMessage Map.! (cid, i) in tap
      , hHoldMessage = \cid i -> let (_, hold, _, _) = perMessage Map.! (cid, i) in hold
      , hTapEntry = \gid i -> fst (perEntry Map.! (gid, i))
      , hHoldEntry = \gid i -> snd (perEntry Map.! (gid, i))
      , hCancelFile = \cid i -> let (_, _, cancel, _) = perMessage Map.! (cid, i) in cancel
      , hSendAgain = \cid i -> let (_, _, _, again) = perMessage Map.! (cid, i) in again
      , hUnselect = unselect
      , hCopySelected = copySelected
      , hForwardSelected = forwardSelected
      , hCancelReply = cancelReply
      , hUnstage = curry perStaged
      , hOpenSettings = openSettings
      , hOpenProfiles = openProfiles
      , hSwitchProfile = \pid -> let (switch', _, _) = perProfile pid in switch'
      , hDeleteProfile = \pid -> let (_, delete, _) = perProfile pid in delete
      , hNewProfile = newProfile
      , hImportProfile = importProfile
      , hOpenBackup = openBackup
      , hOpenDebug = openDebug
      , hExportDebug = exportDebug
      , hBackUp = backUp
      , hToggleBackup = \pid -> let (_, _, toggle) = perProfile pid in toggle
      , hRestore = restoreBackup
      , hPickAvatar = pickAvatar
      , hRemoveAvatar = removeAvatar
      , hExport = exportProfile
      , hAutoAccept = chooseAutoAccept
      , hAway = chooseAway
      , hToggleUdp = toggleUdp
      , hToggleIpv6 = toggleIpv6
      , hToggleLan = toggleLan
      , hToggleStay = toggleStay
      , hToggleTyping = toggleTyping
      , hOpenProxy = openProxy
      , hProxyKind = \case
          Nothing               -> let (none, _, _, _) = proxyKinds in none
          Just Tox.Socks5Proxy  -> let (_, socks, _, _) = proxyKinds in socks
          Just Tox.HttpProxy    -> let (_, _, http, _) = proxyKinds in http
          Just Tox.GatewayProxy -> let (_, _, _, gateway) = proxyKinds in gateway
      , hProxyHost = proxyHost
      , hProxyPort = proxyPort
      , hApplyProxy = applyProxy
      , hForwardTo = caForwardTo . perContact
      , hOpenChat = caOpen . perContact
      , hAccept = caAccept . perContact
      , hReject = caReject . perContact
      , hDetails = caDetails . perContact
      , hMuteChat = caMute . perContact
      , hDelete = caDelete . perContact
      , hHoldChat = caHold . perContact
      , hCopyKey = caCopyKey . perContact
      , hPictures = caPictures . perContact
      , hDraft = draft
      , hNickname = nickname
      , hStatus = status
      , hAddress = address
      , hRequest = request
      , hCall = \camera -> (if camera then snd else fst) calls
      , hAnswer = \camera -> (if camera then snd else fst) answers
      , hHangUp = hangUp
      , hMute = mute
      , hCamera = camera'
      , hRoute = route
      , hFlipCamera = flipCamera
      , hHideCall = hideCall
      , hShowCall = showCall
      , hReturnToCall = returnToCall
      , hScrolled = scrolled
      , hJump = jump
      , hOpenGroup = gaOpen . perGroup
      , hGroupDetails = gaDetails . perGroup
      , hMuteGroup = gaMute . perGroup
      , hLeaveGroup = gaLeave . perGroup
      , hDeleteGroup = gaDelete . perGroup
      , hHoldGroup = gaHold . perGroup
      , hCopyChatId = gaCopyId . perGroup
      , hOpenInvite = \cid gid -> iaOpen (perInvite (cid, gid))
      , hJoin = \cid gid -> iaJoin (perInvite (cid, gid))
      , hDecline = \cid gid -> iaDecline (perInvite (cid, gid))
      , hPassword = password
      , hEditTopic = gaTopic . perGroup
      , hToggleMentions = gaMentions . perGroup
      , hTopic = topic
      , hApplyTopic = applyTopic
      , hFounding = (foundings Map.!)
      , hGroupName = groupName'
      , hFound = found
      , hChatId = chatId
      , hJoinPublic = joinPublic
      , hInviteFriends = gaInvite . perGroup
      , hInviteFriend = curry perInviting
      , hMember = curry perMember
      , hGroupFile = \gid fid -> perGroupFile (gid, fid, False)
      , hPauseGroupFile = \gid fid -> perGroupFile (gid, fid, True)
      , hEndPrivate = endPrivate
      , hGroupSettings = gaSettings . perGroup
      , hPrivacy = gaPrivacy . perGroup
      , hTopicLock = gaTopicLock . perGroup
      , hGroupPictures = gaPictures . perGroup
      , hVoice = gaVoice . perGroup
      , hRelayBot = gaRelayBot . perGroup
      , hStopRelay = gaStopRelay . perGroup
      , hPeerLimit = peerLimit
      , hGroupPassword = groupPassword
      , hApplyGroupSettings = applyGroupSettings
      , hOpenLink = openLink
      , hLinkEdited = linkEdited
      , hLink = link
      , hUnlock = unlock
      , hOpenDevices = openDevices
      , hPairDevice = pairDevice
      , hCopyPairing = copyPairing
      , hRemoveDevice = perDevice
      }
  let redrawn = join (readIORef redraw)
      muting cid = do
        now <- getCurrentTime
        contact <- Map.lookup cid . clientContacts . modelClient <$> readMVar model
        case contact of
          Just c | muted now c -> fire (ClientEvent (Client.UnmuteChat cid)) >> redrawn
          Just _ -> readIORef user >>= mapM_ (\u ->
            showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Mute notifications" (map fst muteChoices)) $ \case
              BottomSheetItemSelected n | (_, for) : _ <- drop (fromIntegral n) muteChoices -> do
                from <- getCurrentTime
                fire (ClientEvent (Client.MuteChat cid ((`addUTCTime` from) <$> for))) >> redrawn
              _ -> pure ())
          Nothing -> pure ()
      deleting cid = do
        contact <- Map.lookup cid . clientContacts . modelClient <$> readMVar model
        forM_ contact $ \c -> readIORef user >>= mapM_ (\u ->
          showDialog (userDialogState u) DialogConfig
            { dcTitle = "Delete " <> contactLabel cid c <> "?"
            , dcMessage = "Their chat can stay, and comes back if you add them again."
            , dcButton1 = "Keep chat"
            , dcButton2 = Just "Delete chat"
            , dcButton3 = Just "Cancel"
            } $ \case
              DialogButton1 -> fire (ClientEvent (Client.DeleteContact cid True)) >> redrawn
              DialogButton2 -> fire (ClientEvent (Client.DeleteContact cid False)) >> redrawn
              _ -> pure ())
      -- Change only what's different from the current settings.
      changing gid f = do
        wanted <- (groupSettings <=< Map.lookup gid) . clientGroups . modelClient <$> readMVar model
        forM_ wanted $ \current -> fire (ClientEvent (Client.ChangeSettings gid (f current))) >> redrawn
      mutingGroup gid = do
        now <- getCurrentTime
        g <- Map.lookup gid . clientGroups . modelClient <$> readMVar model
        case g of
          Just g' | groupMuted now g' -> fire (ClientEvent (Client.UnmuteGroup gid)) >> redrawn
          Just _ -> withUser $ \u ->
            showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Mute notifications" (map fst muteChoices)) $ \case
              BottomSheetItemSelected n | (_, for) : _ <- drop (fromIntegral n) muteChoices -> do
                from <- getCurrentTime
                fire (ClientEvent (Client.MuteGroup gid ((`addUTCTime` from) <$> for))) >> redrawn
              _ -> pure ()
          Nothing -> pure ()
      -- For a friend's chat or a group.
      choosingPictures sendIn = withUser $ \u ->
        let ways = ("As they are", Nothing) : [ ("As " <> formatName f, Just (formatKey f)) | f <- pictureFormats ]
        in showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Send pictures" (map fst ways)) $ \case
             BottomSheetItemSelected n | (_, way) : _ <- drop (fromIntegral n) ways -> fire (ClientEvent (sendIn way)) >> redrawn
             _ -> pure ()
      leavingGroup gid = do
        kind <- maybe "group" (kindName . groupKind) . Map.lookup gid . clientGroups . modelClient <$> readMVar model
        asking ("Leave the " <> kind <> "?") "Its chat stays here. To join again, ask for a new invite."
          "Leave" (ClientEvent (Client.LeaveGroup gid))
      deletingGroup gid = asking "Delete the chat?" "Its messages go too, and you leave the group if you're in it."
        "Delete" (ClientEvent (Client.DeleteGroup gid))
      -- A chat's menu in the chat list.
      holding title choices = withUser $ \u ->
        showBottomSheet (userBottomSheetState u) (BottomSheetConfig title (map fst choices)) $ \case
          BottomSheetItemSelected n | (_, chosen) : _ <- drop (fromIntegral n) choices -> chosen
          _ -> pure ()
      asking title message yes event = withUser $ \u ->
        showDialog (userDialogState u) DialogConfig
          { dcTitle = title, dcMessage = message, dcButton1 = yes, dcButton2 = Just "Cancel", dcButton3 = Nothing } $ \case
            DialogButton1 -> fire event >> redrawn
            _ -> pure ()
  groupsMade <- newIORef Map.empty
  let groupActionsFor gid@(GroupId chat) = readIORef groupsMade >>= \known -> case Map.lookup gid known of
        Just these -> pure these
        Nothing -> do
          these <- runActionM actions $ GroupActions
            <$> createAction (fire (Open (GroupChat gid)))
            <*> createAction (fire (Open (GroupDetails gid)))
            <*> createAction (mutingGroup gid)
            <*> createAction (leavingGroup gid)
            <*> createAction (deletingGroup gid)
            <*> createAction (copyToClipboard chat)
            <*> createAction (fire (Open (TopicScreen gid)))
            <*> createAction (fire (ClientEvent (Client.ToggleMentions gid)))
            <*> createAction (fire (Open (InviteFriendsScreen gid)))
            <*> createAction (fire (Open (GroupSettingsScreen gid)))
            <*> createAction (changing gid (\s -> s { Tox.groupPrivate = not (Tox.groupPrivate s) }))
            <*> createAction (changing gid (\s -> s { Tox.groupTopicLock = not (Tox.groupTopicLock s) }))
            <*> createAction (withUser $ \u ->
                  showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Who may speak" (map fst voices)) $ \case
                    BottomSheetItemSelected n | (_, v) : _ <- drop (fromIntegral n) voices ->
                      changing gid (\s -> s { Tox.groupVoice = v })
                    _ -> pure ())
            <*> createAction (do
                  current <- readMVar model
                  forM_ (Map.lookup gid (clientGroups (modelClient current))) $ \g -> withUser $ \u -> do
                    let senders = relayCounts g
                        choice (m, relays, total) =
                          memberLabel m <> ": " <> Text.pack (show relays) <> " of " <> Text.pack (show total) <> " messages relayed"
                    showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Which member is the relay bot?" (map choice senders)) $ \case
                      BottomSheetItemSelected n | (m, _, _) : _ <- drop (fromIntegral n) senders ->
                        fire (ClientEvent (Client.ChooseRelayBot gid (Just (memberKey m)))) >> redrawn
                      _ -> pure ())
            <*> createAction (fire (ClientEvent (Client.ChooseRelayBot gid Nothing)))
            <*> createAction (choosingPictures (Client.SendGroupPictures gid))
            <*> createAction (do
                  now <- getCurrentTime
                  g <- Map.lookup gid . clientGroups . modelClient <$> readMVar model
                  forM_ g $ \g' -> holding (groupLabel g') $
                    (if groupMuted now g' then "Unmute notifications" else "Mute notifications", mutingGroup gid)
                    : [ ("Leave " <> kindName (groupKind g'), leavingGroup gid) | groupPhase g' `elem` [Joining, InGroup] ]
                    ++ [("Delete chat", deletingGroup gid)])
          modifyIORef' groupsMade (Map.insert gid these)
          pure these
  invitesMade <- newIORef Map.empty
  -- Per friend and group, made once the screen shows them.
  invitingMade <- newIORef Map.empty
  -- X button of each staged item, by chat and index.
  unstagingMade <- newIORef Map.empty
  let unstagingFor at@(cid, k) = readIORef unstagingMade >>= \known -> case Map.lookup at known of
        Just this -> pure this
        Nothing -> do
          this <- runActionM actions (createAction (fire (ClientEvent (Client.Unstage cid k))))
          modifyIORef' unstagingMade (Map.insert at this)
          pure this
  -- Per file shared in a group, downloading it or pausing that.
  groupFilesMade <- newIORef Map.empty
  let groupFileFor at@(gid, fid, pausing) = readIORef groupFilesMade >>= \known -> case Map.lookup at known of
        Just this -> pure this
        Nothing -> do
          this <- runActionM actions (createAction (fire (ClientEvent ((if pausing then Client.PauseGroupFile else Client.DownloadGroupFile) gid fid))))
          modifyIORef' groupFilesMade (Map.insert at this)
          pure this
  -- Per member, offering what we may do to them now.
  membersMade <- newIORef Map.empty
  let memberFor at@(gid, n) = readIORef membersMade >>= \known -> case Map.lookup at known of
        Just this -> pure this
        Nothing -> do
          this <- runActionM actions $ createAction $ do
            g <- Map.lookup gid . clientGroups . modelClient <$> readMVar model
            forM_ g $ \g' -> withUser $ \u -> do
              let theirs = Map.findWithDefault Tox.User n (groupRoles g')
                  choices = ("Message privately", fire (PrivatelyTo (memberKey <$> Map.lookup n (groupMembers g'))) >> fire (Open (GroupChat gid)))
                    : [ (memberActionName permission, fire (ClientEvent (Client.Moderate gid n permission)))
                      | groupKind g' == NgcGroup, permission <- memberActions (groupSelfRole g') theirs ]
                  title = maybe "Member" memberLabel (Map.lookup n (groupMembers g'))
              showBottomSheet (userBottomSheetState u) (BottomSheetConfig title (map fst choices)) $ \case
                BottomSheetItemSelected k | (_, chosen) : _ <- drop (fromIntegral k) choices -> chosen >> redrawn
                _ -> pure ()
          modifyIORef' membersMade (Map.insert at this)
          pure this
  let invitingFor at@(gid, cid) = readIORef invitingMade >>= \known -> case Map.lookup at known of
        Just this -> pure this
        Nothing -> do
          this <- runActionM actions (createAction (fire (ClientEvent (Client.InviteFriend gid cid))))
          modifyIORef' invitingMade (Map.insert at this)
          pure this
  let inviteActionsFor at@(cid, gid) = readIORef invitesMade >>= \known -> case Map.lookup at known of
        Just these -> pure these
        Nothing -> do
          these <- runActionM actions $ InviteActions
            <$> createAction (fire (Open (InviteScreen cid gid)))
            <*> createAction (fire (AnswerInvite cid gid True))
            <*> createAction (fire (AnswerInvite cid gid False))
          modifyIORef' invitesMade (Map.insert at these)
          pure these
  -- Actions live as long as the app, so make each contact's once.
  made <- newIORef Map.empty
  let actionsFor cid = do
        known <- readIORef made
        case Map.lookup cid known of
          Just these -> pure these
          Nothing -> do
            these <- runActionM actions $ ContactActions
              <$> createAction (fire (Open (Chat cid)))
              <*> createAction (fire (ClientEvent (Client.AcceptRequest cid)))
              <*> createAction (fire (ClientEvent (Client.RejectRequest cid)))
              <*> createAction (fire (Open (Details cid)))
              <*> createAction (muting cid)
              <*> createAction (deleting cid)
              <*> createAction (copyToClipboard (let ContactId key = cid in key))
              <*> createAction (getCurrentTime >>= fire . ForwardTo cid)
              -- From the chat (the call screen then covers it).
              <*> createAction (allowing False (\allowed -> do
                    fire (Open (Chat cid))
                    getCurrentTime >>= \now -> fire (StartCall now allowed)))
              <*> createAction (choosingPictures (Client.SendPictures cid))
              <*> createAction (do
                    now <- getCurrentTime
                    contact <- Map.lookup cid . clientContacts . modelClient <$> readMVar model
                    forM_ contact $ \c -> holding (contactLabel cid c)
                      [ (if muted now c then "Unmute notifications" else "Mute notifications", muting cid)
                      , ("Delete contact", deleting cid) ])
            modifyIORef' made (Map.insert cid these)
            pure these
  -- Per message, once shown. A tap selects while others are selected, or
  -- opens or accepts a file. Long press opens the menu.
  messageActions <- newIORef Map.empty
  let messageAction ref@(cid, i) = do
        known <- readIORef messageActions
        case Map.lookup ref known of
          Just these -> pure these
          Nothing -> do
            let message current = Map.lookup cid (clientContacts (modelClient current)) >>= find ((== i) . messageId) . contactMessages
                chat = Client.FriendChat cid
            these <- runActionM actions $ (,,,)
              <$> createAction (do
                current <- readMVar model
                if not (Set.null (modelSelected current)) then fire (Select i)
                else forM_ (message current) $ \m -> do
                 forM_ (location (shownText (messageText m))) $ \(latitude, longitude) ->
                   when (isNothing (messageFile m)) $ openUri (mapsUri latitude longitude)
                 forM_ (messageFile m) $ \file -> case attachmentState file of
                  Complete                      -> openFile (attachmentPath file)
                  Waiting | not (messageMine m) -> fire (ClientEvent (Client.Download cid i))
                  _                             -> pure ())
              <*> createAction (do
                current <- readMVar model
                forM_ (message current) $ \m -> do
                  let call = isJust (messageCall m)
                      choices =
                        [ ("Open " <> written, openUri uri)
                        | not call, isNothing (messageFile m), (written, uri) <- take 3 (findLinks (shownText (messageText m))) ]
                        ++ [("Reply", fire (Reply i)) | not call]
                        ++ [("Forward", fire (Open (Forward chat [i]))) | forwardable m]
                        ++ [("Copy", copyToClipboard (copiedText (modelClient current) chat [i])) | not call]
                        ++ [("Select", fire (Select i)), ("Info", fire (Open (MessageInfo chat i)))]
                  readIORef user >>= mapM_ (\u ->
                    showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Message" (map fst choices)) $ \case
                      BottomSheetItemSelected n | (_, chosen) : _ <- drop (fromIntegral n) choices -> chosen >> redrawn
                      _ -> pure ()))
              <*> createAction (fire (ClientEvent (Client.Cancel cid i)))
              <*> createAction (fire (ClientEvent (Client.SendAgain cid i)))
            modifyIORef' messageActions (Map.insert ref these)
            pure these
  -- Per group message, as for a friend's. Only what members wrote.
  entryActions <- newIORef Map.empty
  let entryAction ref@(gid, i) = do
        known <- readIORef entryActions
        case Map.lookup ref known of
          Just these -> pure these
          Nothing -> do
            let chat = Client.GroupChat gid
                entry current = find ((== i) . entryId) . groupEntries =<< Map.lookup gid (clientGroups (modelClient current))
            these <- runActionM actions $ (,)
              <$> createAction (do
                current <- readMVar model
                unless (Set.null (modelSelected current)) (fire (Select i)))
              <*> createAction (do
                current <- readMVar model
                forM_ (entry current) $ \e -> forM_ (entryText e) $ \text -> do
                  -- An answer may go to everyone, so not to what was private.
                  let public = case entryWhat e of
                        Said {} -> True
                        _       -> False
                      choices =
                        [ ("Open " <> written, openUri uri) | (written, uri) <- take 3 (findLinks text) ]
                        ++ [ ("Reply", fire (Reply i)) | public ]
                        ++ [ ("Forward", fire (Open (Forward chat [i])))
                           , ("Copy", copyToClipboard (copiedText (modelClient current) chat [i]))
                           , ("Select", fire (Select i)), ("Info", fire (Open (MessageInfo chat i))) ]
                  readIORef user >>= mapM_ (\u ->
                    showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Message" (map fst choices)) $ \case
                      BottomSheetItemSelected n | (_, chosen) : _ <- drop (fromIntegral n) choices -> chosen >> redrawn
                      _ -> pure ()))
            modifyIORef' entryActions (Map.insert ref these)
            pure these
  device <- getDeviceInfo
  -- Identicons, made once at the largest size we show.
  avatars <- newIORef Map.empty
  let avatarSize = dp (theme (diScreenDensity device) 0 False) 48
      avatarOf key = atomicModifyIORef' avatars $ \known ->
        case Map.lookup key known of
          Just picture -> (known, picture)
          Nothing ->
            let picture = identiconPng (ceiling avatarSize) (identicon (unhex key))
            in (Map.insert key picture known, picture)
  -- Cached, as we only ask once the picture is complete.
  pictureSizes <- newIORef Map.empty
  let sizeOf path = readIORef pictureSizes >>= \known -> case Map.lookup path known of
        Just size -> pure size
        Nothing -> do
          size <- if isQoi path
            then either (const Nothing) qoiSize <$> try @IOException (withFile path ReadMode (`BS.hGet` 14))
            else imageSize path
          modifyIORef' pictureSizes (Map.insert path size)
          pure size
  -- QOI pictures as bitmaps, decoded off the UI thread. Nothing while
  -- decoding, or if it isn't QOI. One at a time (each may take as much memory
  -- as the largest picture).
  decoded <- newIORef (Map.empty :: Map.Map FilePath (Maybe BS.ByteString))
  decoding <- newMVar ()
  let decodedOf path = do
        known <- readIORef decoded
        unless (Map.member path known) $ do
          modifyIORef' decoded (Map.insert path Nothing)
          void . forkNamed "htox picture decoding" . withMVar decoding $ \() -> do
            bitmap <- try @IOException (BS.readFile path)
            let shown = either (const Nothing) (either (const Nothing) Just . qoiBitmap (diScreenWidth device)) bitmap
            forM_ shown $ \b -> do
              -- Do the work here, not in the view.
              b' <- evaluate b
              atomicModifyIORef' decoded ((, ()) . Map.insert path (Just b')) >> join (readIORef redraw)
        pure (join (Map.lookup path known))
  -- Removing each of the account's devices, after asking.
  deviceActions <- newIORef Map.empty
  let deviceActionFor cid = readIORef deviceActions >>= \known -> case Map.lookup cid known of
        Just this -> pure this
        Nothing -> do
          this <- runActionM actions $ createAction $ withUser $ \u ->
            showDialog (userDialogState u) DialogConfig
              { dcTitle = "Remove this device?"
              , dcMessage = "It stops getting this profile's chats. Link it again to have them back."
              , dcButton1 = "Remove"
              , dcButton2 = Just "Cancel"
              , dcButton3 = Nothing
              } $ \case
                DialogButton1 -> fire (ClientEvent (Client.RemoveDevice cid)) >> redrawn
                _ -> pure ()
          modifyIORef' deviceActions (Map.insert cid this)
          pure this
  -- Per account: switch to it, delete it, and choose it for a backup.
  profileActions <- newIORef Map.empty
  let profileActionsFor pid = readIORef profileActions >>= \known -> case Map.lookup pid known of
        Just these -> pure these
        Nothing -> do
          these <- runActionM actions $ (,,)
            <$> createAction (switching (pure pid))
            <*> createAction (withUser $ \u -> do
                  name <- maybe "this profile" accountName . find ((== pid) . accountId) . modelProfiles <$> readMVar model
                  showDialog (userDialogState u) DialogConfig
                    { dcTitle = "Delete " <> name <> "?"
                    , dcMessage = "Its chats and keys go too. Back it up first to keep it."
                    , dcButton1 = "Delete"
                    , dcButton2 = Just "Cancel"
                    , dcButton3 = Nothing
                    } $ \case
                      DialogButton1 -> do
                        goOffline hub pid
                        Store.deleteProfile database pid
                        Store.profiles database >>= fire . ProfilesChanged
                        redrawn
                      _ -> pure ())
            <*> createAction (fire (ToggleBackup pid))
          modifyIORef' profileActions (Map.insert pid these)
          pure these
      taken path = do
        bytes <- BS.readFile path
        void (try @IOException (removeFile path >> removeDirectory (takeDirectory path)))
        taking bytes
      -- Restore the backup's profiles we don't have, online at once.
      restored path = do
        result <- try @SomeException (Store.restore database path)
        void (try @IOException (removeFile path >> removeDirectory (takeDirectory path)))
        case result of
          Left err -> do
            platformLog ("No restore: " <> Text.pack (displayException err))
            fire (Notify (case fromException err of
              Just (Store.Newer _) -> "That backup is from a newer hTox"
              Nothing              -> "That isn't an hTox backup")) >> redrawn
          Right [] -> fire (Notify "hTox has those profiles already") >> redrawn
          Right pids -> do
            forM_ pids $ \pid -> try @SomeException (sessionFor hub pid) >>= \case
              Left err -> platformLog ("A profile didn't go online: " <> Text.pack (displayException err))
              Right _ -> pure ()
            Store.profiles database >>= fire . ProfilesChanged
            redrawn
  let forProfile key f = do
        let (whose, key') = profileOf key
            pid = fromMaybe shownId whose
        sessions <- readMVar (hubSessions hub)
        forM_ (Map.lookup pid sessions) $ \s -> f pid s key'
      showing pid = if pid == shownId then join (readIORef redraw) else switching (pure pid)
  -- Only made again when the Tox ID changes.
  qr <- newIORef (Text.empty, BS.empty)
  let qrOf address = atomicModifyIORef' qr $ \known@(shown, picture) ->
        if shown == address then (known, picture)
        else let new = toxIdQr (ceiling (dp (theme (diScreenDensity device) 0 False) 200)) address
             in ((address, new), new)
  pure (MobileApp
    { maContext = loggingMobileContext
        { onBack = readMVar model >>= \current ->
            if isJust (back current) then True <$ fire Back else pure False
        , onLifecycle = \case
            -- Also when back from the full screen setting.
            Start -> fullScreenAllowed >>= fire . FullScreenChecked >> fire (Shown True)
            -- The system may end hTox without a word from here on.
            Stop -> fire (Shown False) >> Store.flush database
            _ -> pure ()
        , onNotificationOpened = \key -> forProfile key $ \pid s key' -> case notificationOf key' of
            -- The call screen.
            Nothing -> showing pid
            Just opened -> do
              client <- modelClient <$> readMVar (sessionModel s)
              let known = case opened of
                    Chat cid             -> Map.member cid (clientContacts client)
                    GroupChat gid        -> Map.member gid (clientGroups client)
                    InviteScreen cid gid -> Map.member (cid, gid) (clientInvites client)
                    _                    -> True
              when known $ sessionFire s (Open opened) >> showing pid
        , onCallButton = \key button -> forProfile key $ \pid s key' -> when (key' == callNotification) $ do
            call <- clientCall . modelClient <$> readMVar (sessionModel s)
            forM_ call $ \c -> case button of
              Answer -> allowing (callPeerVideo c) $ \allowed -> do
                now <- getCurrentTime
                sessionFire s (ClientEvent (Client.AnswerCall now allowed)) >> showing pid
              Decline -> getCurrentTime >>= sessionFire s . ClientEvent . Client.HangUp >> join (readIORef redraw)
              End -> getCurrentTime >>= sessionFire s . ClientEvent . Client.HangUp >> join (readIORef redraw)
        , onPicturePicked = fire . ClientEvent . Client.SetAvatar
        , onFilePicked = \picked -> do
            chosen <- readIORef picking
            if chosen == ForProfile then taken (pickedPath picked)
            else if chosen == ForBackup then restored (pickedPath picked) else do
              -- Move it to our files (the system may clear the cache).
              now <- getCurrentTime
              let path = sent </> (show (toMillis now) <> "-" <> takeFileName (pickedPath picked))
              renameFile (pickedPath picked) path
              -- Remove the directory it was copied to, if it's empty.
              void (try (removeDirectory (takeDirectory (pickedPath picked))) :: IO (Either IOException ()))
              fileId <- randomByteString 32
              current <- readMVar model
              let shrinkable = chosen == ForPicture && map toLower (takeExtension path) /= ".gif"
                  way = case modelScreen current of
                    Chat cid -> pictureFormat =<< contactPictures =<< Map.lookup cid (clientContacts (modelClient current))
                    GroupChat gid -> pictureFormat =<< groupPictures =<< Map.lookup gid (clientGroups (modelClient current))
                    _        -> Nothing
                  attach p name size = fire (Attach now p name size fileId) >> redrawn
                  original = attach path (pickedName picked) (fromInteger (pickedSize picked))
                  instead extension write = do
                    let converted = dropExtension path <> extension
                    written <- write converted
                    if not written then original else do
                      -- A JPEG shrunk to JPEG has replaced it already.
                      when (converted /= path) (removeFile path)
                      size <- withFile converted ReadMode hFileSize
                      attach converted (Text.pack (dropExtension (Text.unpack (pickedName picked)) <> extension)) (fromInteger size)
              -- Off the UI thread. Send it as it is if that fails.
              if not shrinkable then original else void . forkNamed "htox picture shrinking" $ case way of
                Nothing -> do
                  let out = dropExtension path <> ".tmp"
                  shrinkPicture path out pictureSide >>= \case
                    Nothing -> original
                    Just kind -> instead (if kind == Jpeg then ".jpg" else ".png") (\converted -> True <$ renameFile out converted)
                Just f -> do
                  encoded <- (>>= \(w, h, rgba) -> formatEncode f w h rgba)
                    <$> picturePixels path (min (formatMaxPixels f) (pictureSide * pictureSide))
                  case encoded of
                    Nothing    -> original
                    Just bytes -> instead (formatExtension f) (\converted -> True <$ BS.writeFile converted bytes)
        }
    , maActionState = actions
    , maView = \state -> do
        writeIORef redraw (userRequestRedraw state)
        writeIORef permissions (Just (userPermissionState state))
        writeIORef user (Just state)
        current <- readMVar model
        let client = modelClient current
            contactIds = Map.keys (clientContacts client) ++ Map.keys (clientRequests client)
        perContact <- Map.fromList <$> traverse (\cid -> (,) cid <$> actionsFor cid) contactIds
        perMessage <- Map.fromList <$> traverse (\ref -> (,) ref <$> messageAction ref)
          [ (cid, i) | Chat cid <- [modelScreen current]
                     , contact <- maybe [] pure (Map.lookup cid (clientContacts client))
                     , i <- map messageId (contactMessages contact) ]
        perEntry <- Map.fromList <$> traverse (\ref -> (,) ref <$> entryAction ref)
          [ (gid, entryId e) | GroupChat gid <- [modelScreen current]
                             , g <- maybe [] pure (Map.lookup gid (clientGroups client))
                             , e <- groupEntries g, isJust (entryText e) ]
        perGroup <- Map.fromList <$> traverse (\gid -> (,) gid <$> groupActionsFor gid) (Map.keys (clientGroups client))
        perInvite <- Map.fromList <$> traverse (\at -> (,) at <$> inviteActionsFor at) (Map.keys (clientInvites client))
        perStaged <- Map.fromList <$> traverse (\at -> (,) at <$> unstagingFor at)
          [ (cid, k) | Chat cid <- [modelScreen current]
                     , k <- [0 .. length (Map.findWithDefault [] cid (clientStaged client)) - 1] ]
        perMember <- Map.fromList <$> traverse (\at -> (,) at <$> memberFor at)
          [ (gid, n) | GroupDetails gid <- [modelScreen current]
                     , g <- maybe [] pure (Map.lookup gid (clientGroups client)), n <- Map.keys (groupMembers g) ]
        perGroupFile <- Map.fromList <$> traverse (\at -> (,) at <$> groupFileFor at)
          [ (gid, groupFileId file, pausing) | GroupChat gid <- [modelScreen current]
                                             , g <- maybe [] pure (Map.lookup gid (clientGroups client))
                                             , GroupEntry _ (SharedFile _ file _) _ <- groupEntries g, pausing <- [False, True] ]
        perInviting <- Map.fromList <$> traverse (\at -> (,) at <$> invitingFor at)
          [ (gid, cid) | InviteFriendsScreen gid <- [modelScreen current]
                       , (cid, c) <- Map.toList (clientContacts client), contactOnline c ]
        perDevice <- Map.fromList <$> traverse (\cid -> (,) cid <$> deviceActionFor cid)
          [ ContactId (Text.toUpper (Text.decodeUtf8 (Base16.encode pk)))
          | DevicesScreen <- [modelScreen current], Just home <- [clientHome client]
          , Sync.PublicKey pk <- Map.keys (Home.devices home) ]
        -- The other profiles, for the drawer. A linked one has no name until
        -- its account's events say it.
        summaries <- readIORef (hubSummaries hub)
        let others = [ (accountId a, if Text.null (summaryName s) then s { summaryName = accountName a } else s)
                     | a <- modelProfiles current, accountId a /= shownId
                     , Just s <- [Map.lookup (accountId a) summaries] ]
        identicons <- traverse avatarOf $ Map.fromList
          [ (key, key)
          | key <- Text.take 64 (profileToxId (clientProfile client))
                 : map (summaryKey . snd) others
                 ++ [key | ContactId key <- contactIds]
                 ++ [accountKey a | a <- modelProfiles current, not (Text.null (accountKey a))]
                 ++ [chat | GroupId chat <- Map.keys (clientGroups client)]
                 ++ [chat | (_, GroupId chat) <- Map.keys (clientInvites client)]
                 ++ [ memberKey m | GroupDetails gid <- [modelScreen current]
                                  , g <- maybe [] pure (Map.lookup gid (clientGroups client))
                                  , m <- Map.elems (groupMembers g), not (Text.null (memberKey m)) ] ]
        -- Avatars, or identicons.
        let pictures = Map.fromList $
              [ (Text.take 64 (profileToxId (clientProfile client)), png)
              | Just png <- [(`Map.lookup` clientAvatars client) =<< clientAvatar client] ]
              ++ [ (key, png) | (ContactId key, c) <- Map.toList (clientContacts client)
                              , Just png <- [(`Map.lookup` clientAvatars client) =<< contactAvatar c] ]
              ++ [ (summaryKey summary, png) | (_, summary) <- others, Just png <- [summaryAvatar summary] ]
            keys = Map.union pictures identicons
        -- The pairing link where it's shown, otherwise the Tox ID.
        code <- qrOf (maybe (profileToxId (clientProfile client)) (Text.drop (Text.length "tox:"))
                        (pairingLink current <* guard (modelScreen current == DevicesScreen)))
        sizes <- traverse sizeOf $ Map.fromList $
          [ (attachmentPath a, attachmentPath a)
          | Chat cid <- [modelScreen current], c <- maybe [] pure (Map.lookup cid (clientContacts client))
          , m <- contactMessages c, Just a <- [messageFile m]
          , messageMine m || attachmentState a == Complete ]
          -- And staged ones.
          ++ [ (path, path) | Chat cid <- [modelScreen current]
                            , StagedFile path _ _ _ <- Map.findWithDefault [] cid (clientStaged client) ]
          -- And files shared in the group, once they're here.
          ++ [ (attachmentPath a, attachmentPath a)
             | GroupChat gid <- [modelScreen current], g <- maybe [] pure (Map.lookup gid (clientGroups client))
             , GroupEntry _ (SharedFile _ file _) _ <- groupEntries g, Just a <- [groupFileData file], attachmentState a == Complete ]
        bitmaps <- Map.traverseWithKey (\path _ -> decodedOf path) (Map.filterWithKey (\path _ -> isQoi path) sizes)
        dark <- isDarkMode
        clock24 <- is24HourClock
        now <- getCurrentTime
        zone <- getCurrentTimeZone
        elsewhere <- readIORef (hubCall hub) >>= \case
          Just (pid, c) | pid /= shownId -> pure (Just c)
          _                              -> pure Nothing
        let env = Env
              { envTheme = theme (diScreenDensity device)
                  (fromIntegral (diScreenWidth device)) dark
              , envNow = now
              , envZone = zone
              , envAvatar = \key -> Map.findWithDefault BS.empty key keys
              , envQr = code
              , envClock24 = clock24
              , envElsewhere = elsewhere
              , envOthers = others
              , envSchema = Store.schemaVersion
              , envGraphics = diGraphics device
              , envPictureSize = \path -> Map.findWithDefault Nothing path sizes
              , envDecoded = \path -> join (Map.lookup path bitmaps)
              }
        perProfile <- Map.fromList <$> traverse (\a -> (,) (accountId a) <$> profileActionsFor (accountId a)) (modelProfiles current)
        pure $ view env (handlers (perContact Map.!) perMessage perEntry (perProfile Map.!) (perGroup Map.!) (perInvite Map.!) (perInviting Map.!) (perMember Map.!) (perStaged Map.!) (perGroupFile Map.!) (perDevice Map.!)) current
    }, stop)
