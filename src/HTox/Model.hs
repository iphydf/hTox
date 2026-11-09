{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | App state, how events change it, and the commands for Tox. Pure, so we
-- can test all the UI logic off the device.
module HTox.Model
  ( ContactId (..)
  , MessageId
  , Contact (..)
  , contactLabel
  , contactOrder
  , GroupId (..)
  , Group (..)
  , GroupPhase (..)
  , GroupKind (..)
  , newGroup
  , MemberAction (..)
  , memberActions
  , aways
  , awayName
  , privateMember
  , Founding (..)
  , foundingError
  , chatIdError
  , Member (..)
  , GroupEntry (..)
  , GroupFile (..)
  , groupFileName
  , Happening (..)
  , Invite (..)
  , groupMuted
  , mentions
  , memberLabel
  , uttered
  , relayCounts
  , relayedThrough
  , groupLabel
  , inviteLabel
  , capitalised
  , kindName
  , ChatRow (..)
  , chatOrder
  , groupNick
  , Mute (..)
  , muted
  , Message (..)
  , Attachment (..)
  , Staged (..)
  , CallRecord (..)
  , FileState (..)
  , FileRef (..)
  , autoAccepted
  , AccountSettings (..)
  , DeviceSettings (..)
  , preview
  , replied
  , forwardable
  , copiedText
  , callText
  , duration
  , interrupted
  , linkError
  , pairingLink
  , Delivery (..)
  , Profile (..)
  , ProfileId (..)
  , Account (..)
  , Connection (..)
  , connectionText
  , ProfileSummary (..)
  , profileSummary
  , Call (..)
  , callTarget
  , CallPhase (..)
  , Glare (..)
  , Route (..)
  , PhoneCall (..)
  , ringTimeout
  , Screen (..)
  , openChat
  , Client (..)
  , Model (..)
  , Event (..)
  , Command (..)
  , Identity (..)
  , toxIdLength
  , toxId
  , unhex
  , addressError
  , requestError
  , profileError
  , requestMessage
  , initial
  , chatPage
  , pageBefore
  , update
  , back
  , clientHolds
  , notifications
  , groupNotifications
  , missedCalls
  , callHistory
  ) where

import           Data.Bifunctor       (first)
import qualified Data.ByteString      as BS
import           Data.Char            (isHexDigit)
import           Data.List            (sortOn)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe, isJust, isNothing,
                                       listToMaybe)
import           Data.Ord             (Down (..))
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Data.Time            (UTCTime)
import           Data.Word            (Word64)
import           Text.Read            (readMaybe)
import qualified Tox.Application.Tox  as Tox
import           Tox.Client
import qualified Tox.Client.Debug     as Debug
import qualified Tox.Client.Home      as Home
import qualified Tox.Client.Linked    as Linked
import           Tox.Client.Locations (location)
import           Tox.Client.Relays    (relayed, relayedNames)
import           Tox.Client.Types
import qualified Tox.Client.Update    as Client
import           Tox.Client.Update    (Command (..), forwardable, interrupted,
                                       memberByKey, replied)
import qualified Tox.Sync.Pairing     as Pairing

-- | Text for the chat list and notifications. Actions get the name in front.
preview :: Text -> Message -> Text
preview name m = case (messageCall m, messageFile m) of
  (Just record, _) -> callText (messageMine m) record
  (_, Just _)      -> "\x1F4CE " <> text
  _ | messageKind m == Tox.MessageAction -> "* " <> name <> " " <> text
    | Just _ <- location text -> "\x1F4CD Location"
    | otherwise -> text
  where text = shownText (messageText m)

copiedText :: Client -> Client.Chat -> [MessageId] -> Text
copiedText client chat is = case said of
  [(_, text)] -> text
  _           -> Text.intercalate "\n" [whose <> ": " <> text | (whose, text) <- said]
  where
    said = case chat of
      Client.FriendChat cid ->
        let contact = Map.lookup cid (clientContacts client)
            whose m
              | messageMine m = profileNickname (clientProfile client)
              | otherwise = maybe "" (contactLabel cid) contact
        in [ (whose m, shownText (messageText m))
           | m <- maybe [] contactMessages contact, messageId m `elem` is, isNothing (messageCall m) ]
      Client.GroupChat gid -> case Map.lookup gid (clientGroups client) of
        Nothing -> []
        Just g ->
          let ours = groupNick (clientProfile client)
              -- Relayed ones by the name relayed.
              whoSaid e = case entryWhat e of
                Said (Just m) _ body _ _ -> Just (uttered (clientProfile client) g m (shownText body))
                Said Nothing _ body _ _  -> Just (ours, shownText body)
                PrivateFrom m _ body     -> Just (memberLabel m, shownText body)
                PrivateTo _ _ body _     -> Just (ours, shownText body)
                _                        -> Nothing
          in [ said' | e <- groupEntries g, entryId e `elem` is, Just said' <- [whoSaid e] ]

callText :: Bool -> CallRecord -> Text
callText mine record =
  maybe kind (\talked -> kind <> " \x00B7 " <> duration (round talked)) (recordDuration record)
  where
    kind = whose <> (if recordVideo record then " video call" else " voice call")
    whose
      | recordMissed record = "Missed"
      | mine = "Outgoing"
      | otherwise = "Incoming"

-- | Minutes and seconds, e.g. 3:07.
duration :: Int -> Text
duration seconds = Text.pack (show (seconds `div` 60)) <> ":" <> Text.justifyRight 2 '0' (Text.pack (show (seconds `mod` 60)))

contactLabel :: ContactId -> Contact -> Text
contactLabel (ContactId key) contact = fromMaybe (Text.take 8 key) (contactName contact)

-- | Latest chat first, then those without messages by name.
contactOrder :: Client -> [(ContactId, Contact)]
contactOrder = sortOn latest . Map.toList . clientContacts
  where
    latest (cid, c) =
      (Down (messageTime <$> listToMaybe (reverse (contactMessages c))), Text.toCaseFold (contactLabel cid c))

-- | Sender and text to show. Messages of the relay bot show as "Name via
-- Bot".
uttered :: Profile -> Group -> Member -> Text -> (Text, Text)
uttered profile g = utter
  where
    known = speakers profile g
    utter who text = case relayed known text of
      Just (name, said) | Just (memberKey who) == groupRelayBot g -> (name <> " via " <> memberLabel who, said)
      _ -> (memberLabel who, text)

-- | Names of everyone who isn't relayed, us included, so we can tell where a
-- relayed name ends.
speakers :: Profile -> Group -> [Text]
speakers profile g = Set.toList $ Set.fromList $ groupNick profile
  : [memberLabel m | GroupEntry _ (Said (Just m) _ _ _ _) _ <- groupEntries g, Just (memberKey m) /= groupRelayBot g]

-- | Members who said anything, with how many of their messages look relayed
-- and how many there are. Likeliest bot first.
relayCounts :: Group -> [(Member, Int, Int)]
relayCounts g = sortOn (\(m, r, n) -> (Down r, Down n, memberLabel m)) $ Map.elems $ Map.fromListWith add
  [ (memberKey m, (m, fromEnum (isJust (relayed [] (shownText text))), 1)) | GroupEntry _ (Said (Just m) _ text _ _) _ <- groupEntries g ]
  where add (m, r, n) (_, r', n') = (m, r + r', n + n')

-- | Names the relay bot relayed, with the last time, latest first.
relayedThrough :: Profile -> Group -> [(Text, UTCTime)]
relayedThrough profile g = relayedNames (speakers profile g)
  [ (time, shownText text) | GroupEntry time (Said (Just m) _ text _ _) _ <- groupEntries g, Just (memberKey m) == groupRelayBot g ]

foundingError :: Founding -> Text -> Maybe Text
foundingError founding name
  | Text.null (Text.strip name) = Just "The name can't be empty"
  | BS.length (Text.encodeUtf8 (Text.strip name)) > limit = Just "The name is too long"
  | otherwise = Nothing
  where
    limit = if founding == NewConference then Tox.maxNameLength else Tox.maxGroupNameSize

chatIdError :: Text -> Maybe Text
chatIdError chatId
  | Text.null chatId = Nothing
  | Text.length chatId /= 64 || not (Text.all isHexDigit chatId) = Just "Must be 64 hexadecimal characters"
  | otherwise = Nothing

kindName :: GroupKind -> Text
kindName = \case
  NgcGroup        -> "group"
  TextConference  -> "conference"
  AudioConference -> "audio conference"

-- | Conferences start without a name.
groupLabel :: Group -> Text
groupLabel g = nameOr (groupName g) (groupKind g)

inviteLabel :: Invite -> Text
inviteLabel invite = nameOr (inviteName invite) (inviteKind invite)

nameOr :: Text -> GroupKind -> Text
nameOr name kind
  | Text.null name = capitalised (kindName kind)
  | otherwise = name

capitalised :: Text -> Text
capitalised word = Text.toUpper (Text.take 1 word) <> Text.drop 1 word

data ChatRow = FriendRow ContactId Contact | GroupRow GroupId Group
  deriving (Eq, Show)

-- | Latest chat first, then those without messages by name.
chatOrder :: Client -> [ChatRow]
chatOrder client = sortOn latest $
  [FriendRow cid c | (cid, c) <- Map.toList (clientContacts client)]
  ++ [GroupRow gid g | (gid, g) <- Map.toList (clientGroups client)]
  where
    latest = \case
      FriendRow cid c ->
        (Down (messageTime <$> listToMaybe (reverse (contactMessages c))), Text.toCaseFold (contactLabel cid c))
      GroupRow _ g ->
        (Down (entryTime <$> listToMaybe (reverse (groupEntries g))), Text.toCaseFold (groupName g))

aways :: [Tox.UserStatus]
aways = [minBound .. maxBound]

awayName :: Tox.UserStatus -> Text
awayName = \case
  Tox.Online -> "Online"
  Tox.Away   -> "Away"
  Tox.Busy   -> "Busy"

connectionText :: Connection -> Text
connectionText = \case
  Offline -> "Connecting\x2026"
  ViaTcp  -> "Online via TCP relay"
  ViaUdp  -> "Online"

-- | What the other profiles show of one, as in their drawer.
data ProfileSummary = ProfileSummary
  { summaryName       :: Text
  , summaryKey        :: Text
  , summaryAvatar     :: Maybe BS.ByteString
    -- ^ PNG.
  , summaryConnection :: Connection
  , summaryUnread     :: Int
    -- ^ In chats that aren't muted.
  } deriving (Eq, Show)

profileSummary :: UTCTime -> Client -> ProfileSummary
profileSummary now client = ProfileSummary
  { summaryName = profileNickname (clientProfile client)
  , summaryKey = Text.take 64 (profileToxId (clientProfile client))
  , summaryAvatar = (`Map.lookup` clientAvatars client) =<< clientAvatar client
  , summaryConnection = clientConnection client
  , summaryUnread = sum [contactUnread c | c <- Map.elems (clientContacts client), not (muted now c)]
      + sum [groupUnread g | g <- Map.elems (clientGroups client), not (groupMuted now g)]
  }

data Route = Earpiece | Speaker | Wired | Bluetooth
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The call as this phone has it.
data PhoneCall = PhoneCall
  { callRoute      :: Route
  , callRoutes     :: [Route]
  , callBackCamera :: Bool
  , callHidden     :: Bool
    -- ^ The user went back to the chats (with a banner to return).
  } deriving (Eq, Show)

-- | Video calls start on the loudspeaker (the phone isn't at the ear).
phoneCall :: Bool -> PhoneCall
phoneCall video = PhoneCall (if video then Speaker else Earpiece) [Earpiece, Speaker] False False

data Screen
  = Contacts
  | Chat ContactId
  | Details ContactId
  | Forward Client.Chat [MessageId]
  | MessageInfo Client.Chat MessageId
  | CallsScreen
  | ProfileScreen
  | SettingsScreen
  | ProfilesScreen
  | ProxyScreen
  | AddContact
  | GroupChat GroupId
  | GroupDetails GroupId
  | InviteScreen ContactId GroupId
    -- ^ Accepting a friend's invite (with the password if it has one).
  | TopicScreen GroupId
  | NewGroupScreen
  | JoinGroupScreen
    -- ^ Joining a public group by its chat ID.
  | InviteFriendsScreen GroupId
  | GroupSettingsScreen GroupId
  | LinkScreen
  | UnlockScreen
    -- ^ The password of a .tox file being imported.
  | DevicesScreen
    -- ^ The account's devices linked here, and pairing a new one.
    -- ^ Linking to an account on another device, by its pairing link.
  | BackupScreen
  | DebugScreen
    -- ^ Where time and memory go.
  deriving (Eq, Show)

data Model = Model
  { modelClient        :: Client
  , modelScreen        :: Screen
  , modelShown         :: Bool
    -- ^ Whether the app is in front of the user.
  , modelDrawer        :: Bool
  , modelPassword      :: Text
    -- ^ Of the open invite, or the public group to join.
  , modelTopic         :: Text
    -- ^ As edited until applied.
  , modelFounding      :: Founding
  , modelGroupName     :: Text
    -- ^ Of the group to found.
  , modelChatId        :: Text
  , modelPrivateTo     :: Maybe Text
    -- ^ Member key the open group chat's draft goes to privately.
  , modelPeerLimit     :: Text
    -- ^ Settings of the open group, as edited until applied.
  , modelGroupPassword :: Text
  , modelNickname      :: Text
    -- ^ As edited on the profile screen until applied, like the status.
  , modelStatus        :: Text
  , modelAddress       :: Text
  , modelRequest       :: Text
  , modelAddError      :: Maybe Text
    -- ^ Why Tox refused the contact last added.
  , modelProxyKind     :: Maybe Tox.ProxyType
    -- ^ As edited until applied, like the host and port.
  , modelProxyHost     :: Text
  , modelProxyPort     :: Text
  , modelFullScreen    :: Bool
    -- ^ Whether calls may ring over the lock screen.
  , modelProfiles      :: [Account]
  , modelAway          :: Bool
    -- ^ Open chat is scrolled away from its newest messages.
  , modelUnseen        :: Int
    -- ^ Messages that came in while scrolled away.
  , modelJump          :: Int
    -- ^ Bumped to scroll the open chat to its newest messages.
  , modelSelected      :: Set MessageId
    -- ^ In the open chat.
  , modelNotice        :: Maybe Text
    -- ^ Shown until the user goes to another screen.
  , modelLink          :: Text
    -- ^ Pairing link as typed.
  , modelDebug         :: Maybe Debug.Report
    -- ^ Measured last, while the debug screen is open.
  , modelBackup        :: Set ProfileId
  , modelPhoneCall     :: PhoneCall
  } deriving (Eq, Show)

phoned :: (PhoneCall -> PhoneCall) -> Model -> Model
phoned f model = model { modelPhoneCall = f (modelPhoneCall model) }

data Event
  = ClientEvent Client.Event
  | Open Screen
  | Back
  | OpenDrawer
  | DraftChanged Text UTCTime
  | Send UTCTime
  | NicknameChanged Text
  | StatusChanged Text
  | ApplyProfile
  | AddressChanged Text
  | RequestChanged Text
  | AddFriend
  | ScrolledAway Bool
  | JumpToEnd
  | Select MessageId
    -- ^ Toggles.
  | Unselect
  | Reply MessageId
  | CancelReply
  | ForwardTo ContactId UTCTime
  | Shown Bool
    -- ^ The app came in front of the user, or went away.
  | FullScreenChecked Bool
  | ProfilesChanged [Account]
  | ProxyKindChosen (Maybe Tox.ProxyType)
  | ProxyHostChanged Text
  | ProxyPortChanged Text
  | ApplyProxy
  | Attach UTCTime FilePath Text Word64 BS.ByteString
    -- ^ Picked for the open chat: path of our copy, name, size and a new
    -- file ID.
  | ShareLocation Double Double
    -- ^ To the open chat.
  | StartCall UTCTime Bool
    -- ^ Call the open chat's friend, with or without the camera.
  | HideCall
  | ShowCall
  | FlipCamera
  | RoutesChanged Route [Route]
    -- ^ Current route, and the ones we could switch to.
  | Leave
    -- ^ Another profile is shown instead. We wait on the chats, with the
    -- call behind its banner.
  | Notify Text
  | PasswordChanged Text
  | AnswerInvite ContactId GroupId Bool
    -- ^ Join with the typed password, or decline.
  | PrivatelyTo (Maybe Text)
    -- ^ Write privately to a member of the open group (by key), or to
    -- everyone again.
  | TopicEdited Text
  | ApplyTopic
  | FoundingChosen Founding
  | GroupNameEdited Text
  | Found
  | ChatIdEdited Text
  | JoinPublic
  | PeerLimitEdited Text
  | GroupPasswordEdited Text
  | ApplyGroupSettings
  | LinkEdited Text
  | ToggleBackup ProfileId
  deriving (Eq, Show)

-- | Proxy as edited, or why it's wrong.
editedProxy :: Model -> Either Text (Maybe Tox.Proxy)
editedProxy model = case modelProxyKind model of
  Nothing -> Right Nothing
  Just kind
    | Text.null (modelProxyHost model) -> Left "Which host is the proxy on?"
    | Just port <- readMaybe (Text.unpack (modelProxyPort model)) :: Maybe Integer
    , port > 0 && port < 65536 ->
        Right (Just (Tox.Proxy kind (Text.unpack (modelProxyHost model)) (fromIntegral port)))
    | otherwise -> Left "The port is a number from 1 to 65535"

addressError :: Text -> Text -> Maybe Text
addressError ours address
  | Text.null address = Nothing
  | Text.length address /= toxIdLength || not (Text.all isHexDigit address) =
      Just $ "Must be " <> Text.pack (show toxIdLength) <> " hexadecimal characters"
  | toxId (Text.take 64 address) (Text.take 8 (Text.drop 64 address))
      /= Text.toUpper address =
      Just "Not a valid Tox ID, please check it for typos"
  | Text.toUpper (Text.take 64 address) == Text.toUpper (Text.take 64 ours) = Just "This is your own Tox ID"
  | otherwise = Nothing

requestError :: Text -> Maybe Text
requestError request
  | Text.null (Text.strip request) = Just "The message can't be empty"
  | BS.length (Text.encodeUtf8 (Text.strip request)) > Tox.maxFriendRequestLength = Just "The message is too long"
  | otherwise = Nothing

profileError :: Text -> Text -> Maybe Text
profileError nickname status
  | bytes nickname > Tox.maxNameLength = Just "The nickname is too long"
  | bytes status > Tox.maxStatusMessageLength = Just "The status message is too long"
  | otherwise = Nothing
  where bytes = BS.length . Text.encodeUtf8 . Text.strip

requestMessage :: Profile -> Text
requestMessage p =
  "Hey, I'm " <> profileNickname p <> ", come chat with me on Tox!"

initial :: Model
initial = Model
  { modelClient = initialClient
  , modelScreen = Contacts
  , modelShown = False
  , modelDrawer = False
  , modelPassword = ""
  , modelTopic = ""
  , modelFounding = PrivateGroup
  , modelGroupName = ""
  , modelChatId = ""
  , modelPrivateTo = Nothing
  , modelPeerLimit = ""
  , modelGroupPassword = ""
  , modelNickname = profileNickname profile
  , modelStatus = profileStatus profile
  , modelAddress = ""
  , modelRequest = requestMessage profile
  , modelAddError = Nothing
  , modelProxyKind = Nothing
  , modelProxyHost = ""
  , modelProxyPort = ""
  , modelFullScreen = True
  , modelProfiles = []
  , modelAway = False
  , modelUnseen = 0
  , modelJump = 0
  , modelSelected = Set.empty
  , modelNotice = Nothing
  , modelLink = ""
  , modelBackup = Set.empty
  , modelDebug = Nothing
  , modelPhoneCall = phoneCall False
  }
  where profile = clientProfile initialClient

-- | Messages of a chat kept in memory besides those still in use, and
-- loaded at once as the user scrolls back.
chatPage :: Int
chatPage = 100

-- | The open chat and its oldest message here, if the store has earlier
-- ones.
pageBefore :: Model -> Maybe (Client.Chat, MessageId)
pageBefore model = case modelScreen model of
  Chat cid | Just c <- Map.lookup cid (clientContacts client), contactEarlier c, m : _ <- contactMessages c ->
    Just (Client.FriendChat cid, messageId m)
  GroupChat gid | Just g <- Map.lookup gid (clientGroups client), groupEarlier g, e : _ <- groupEntries g ->
    Just (Client.GroupChat gid, entryId e)
  _ -> Nothing
  where client = modelClient model

update :: Event -> Model -> (Model, [Command])
update event model = measuring . leaving $ case event of
  ClientEvent e -> told e model
  Open screen -> reading $ opened screen model
    { modelScreen = screen, modelDrawer = False, modelNotice = Nothing, modelSelected = Set.empty
    , modelAway = False, modelUnseen = 0
    -- Keep the private recipient when going between a group's chat and its
    -- page.
    , modelPrivateTo = case (modelScreen model, screen) of
        (GroupDetails a, GroupChat b) | a == b -> modelPrivateTo model
        (GroupChat a, GroupDetails b) | a == b -> modelPrivateTo model
        _                                      -> Nothing }
  LinkEdited link -> only model { modelLink = link, modelNotice = Nothing }
  ToggleBackup pid -> only model
    { modelBackup = (if Set.member pid (modelBackup model) then Set.delete else Set.insert) pid (modelBackup model) }
  Back -> reading $ fromMaybe model (back model { modelNotice = Nothing })
  Notify notice -> only model { modelNotice = Just notice }
  ShareLocation latitude longitude -> staging (StagedPlace latitude longitude) model { modelNotice = Nothing }
  OpenDrawer -> only model { modelDrawer = True }
  DraftChanged draft time -> case openChat model of
    Just chat@(Client.FriendChat cid) | not (Text.null (Text.strip draft)) ->
      let (drafted, commands) = told (Client.SetDraft chat draft) model
      in (commands <>) <$> told (Client.Typing cid time) drafted
    chat -> maybe (only model) (\c -> told (Client.SetDraft c draft) model) chat
  Send time -> case openChat model of
    Just chat@(Client.GroupChat gid)
      | Just key <- modelPrivateTo model ->
          if isJust (Map.lookup gid (clientGroups client) >>= memberByKey key)
            then sending chat (Client.SendPrivately gid key time)
            else only model { modelNotice = Just privateGone }
    Just chat -> sending chat (Client.Send chat time)
    Nothing -> only model
  NicknameChanged nickname -> only model { modelNickname = nickname }
  StatusChanged status -> only model { modelStatus = status }
  ApplyProfile
    | Nothing <- profileError (modelNickname model) (modelStatus model) ->
        told (Client.ChangeProfile (modelNickname model) (modelStatus model)) model
    | otherwise -> only model
  AddressChanged address -> only model { modelAddress = withoutScheme (Text.strip address), modelAddError = Nothing }
  RequestChanged request -> only model { modelRequest = request }
  AddFriend
    | not (Text.null address)
    , Nothing <- addressError (profileToxId (clientProfile client)) address
    , Nothing <- requestError (modelRequest model) ->
        told (Client.AddFriend address (modelRequest model))
          model { modelScreen = Chat (ContactId (Text.toUpper (Text.take 64 address))), modelAddress = "" }
    | otherwise -> only model
    where address = modelAddress model
  Select i -> only model
    { modelSelected = (if Set.member i (modelSelected model) then Set.delete else Set.insert) i (modelSelected model) }
  Unselect -> only model { modelSelected = Set.empty }
  ScrolledAway away
    | away -> only model { modelAway = True }
    | otherwise -> reading model { modelAway = False, modelUnseen = 0 }
  JumpToEnd -> only model { modelJump = modelJump model + 1, modelUnseen = 0 }
  Reply i -> case openChat model of
    Just chat -> told (Client.ReplyTo chat i) model { modelSelected = Set.empty }
    Nothing   -> only model
  CancelReply -> case openChat model of
    Just chat -> told (Client.CancelReply chat) model
    Nothing   -> only model
  ForwardTo target time -> case modelScreen model of
    Forward source is | Map.member target (clientContacts client) ->
      told (Client.Forward source is target time) model { modelScreen = Chat target, modelSelected = Set.empty }
    _ -> only model
  Attach time path name size fileId -> case modelScreen model of
    Chat _ -> staging (StagedFile path name size fileId) model
    -- Shared at once: a group has nothing to stage it in.
    GroupChat gid -> let (m, commands) = told (Client.WroteGroupFile gid time path name size) model
                     in (m { modelJump = modelJump m + 1, modelUnseen = 0 }, commands)
    -- The user left the chat while picking, so drop the copy.
    _      -> (model, [ForgetFile path])
  StartCall time camera -> case modelScreen model of
    Chat cid -> told (Client.StartCall cid time camera) model
    _        -> only model
  HideCall -> only $ fromMaybe model (back model)
  ShowCall -> only (phoned (\c -> c { callHidden = False }) model)
  FlipCamera -> only (phoned (\c -> c { callBackCamera = not (callBackCamera c) }) model)
  RoutesChanged route routes -> only (phoned (\c -> c { callRoute = route, callRoutes = routes }) model)
  Leave -> first (phoned (\c -> c { callHidden = True })) (update (Open Contacts) model)
  FullScreenChecked allowed -> only model { modelFullScreen = allowed }
  -- Any notice on screen was about the profiles before.
  ProfilesChanged accounts -> only model { modelProfiles = accounts, modelNotice = Nothing }
  ProxyKindChosen kind -> only model { modelProxyKind = kind, modelNotice = Nothing }
  ProxyHostChanged host -> only model { modelProxyHost = Text.strip host, modelNotice = Nothing }
  ProxyPortChanged port -> only model { modelProxyPort = Text.strip port, modelNotice = Nothing }
  ApplyProxy -> case editedProxy model of
    Left why -> only model { modelNotice = Just why }
    Right proxy ->
      told (Client.SetDevice (clientDevice client) { settingProxy = proxy }) model { modelScreen = SettingsScreen }
  Shown shown
    | shown -> reading model { modelShown = True }
    | otherwise -> only model { modelShown = False }
  PasswordChanged password -> only model { modelPassword = password }
  AnswerInvite cid gid join
    | Map.member (cid, gid) (clientInvites client) ->
        told (Client.AnswerInvite cid gid (if join then Just (modelPassword model) else Nothing))
          model { modelScreen = if join then GroupChat gid else Contacts, modelPassword = "" }
    | otherwise -> only model
  PrivatelyTo to -> only model { modelPrivateTo = to }
  PeerLimitEdited limit -> only model { modelPeerLimit = limit }
  GroupPasswordEdited password -> only model { modelGroupPassword = password }
  ApplyGroupSettings -> case modelScreen model of
    GroupSettingsScreen gid
      | Just g <- Map.lookup gid (clientGroups client), Just settings <- groupSettings g ->
          case readMaybe (Text.unpack (Text.strip (modelPeerLimit model))) :: Maybe Integer of
            Just limit | limit > 0 && limit < 65536
                       , BS.length (Text.encodeUtf8 (modelGroupPassword model)) <= Tox.maxPasswordSize ->
              told (Client.ChangeSettings gid settings { Tox.groupPeerLimit = fromIntegral limit
                                                       , Tox.groupPassword = Text.encodeUtf8 (modelGroupPassword model) })
                model { modelScreen = GroupDetails gid }
            _ -> only model { modelNotice = Just "The limit is a number from 1 to 65535, the password at most 32 bytes" }
    _ -> only model
  TopicEdited topic -> only model { modelTopic = topic }
  ApplyTopic -> case modelScreen model of
    TopicScreen gid
      | Just g <- Map.lookup gid (clientGroups client), groupPhase g == InGroup, groupMayTopic g ->
          told (Client.SetTopic gid (modelTopic model)) model { modelScreen = GroupDetails gid }
    _ -> only model
  FoundingChosen founding -> only model { modelFounding = founding }
  GroupNameEdited name -> only model { modelGroupName = name }
  Found
    | Nothing <- foundingError (modelFounding model) (modelGroupName model) ->
        told (Client.Found (modelFounding model) (modelGroupName model)) model { modelScreen = Contacts }
    | otherwise -> only model { modelNotice = foundingError (modelFounding model) (modelGroupName model) }
  ChatIdEdited chatId -> only model { modelChatId = Text.toUpper (Text.strip chatId) }
  JoinPublic
    | not (Text.null chatId), Nothing <- chatIdError chatId ->
        told (Client.JoinPublic (GroupId chatId) (modelPassword model))
          model { modelScreen = GroupChat (GroupId chatId), modelChatId = "", modelPassword = "" }
    | otherwise -> only model
    where chatId = modelChatId model
  where
    client = modelClient model
    only m = (m, [])
    -- A chat the user left keeps only its newest in memory.
    leaving (m, commands) = case openChat model of
      Just chat | openChat m /= Just chat -> (++) commands <$> told (Client.CloseChat chat) m
      _ -> (m, commands)
    -- As the debug screen opens, and each second while it's open.
    measuring (m, commands)
      | modelScreen m == DebugScreen, event == Open DebugScreen || isTick = (m, commands ++ [MeasureDebug])
      | otherwise = (m, commands)
    isTick = case event of
      ClientEvent (Client.Tick _) -> True
      _                           -> False
    sending chat e
      | Client.drafted chat client = let (m, commands) = told e model in (m { modelJump = modelJump m + 1, modelUnseen = 0 }, commands)
      | otherwise = only model
    staging item m = case modelScreen m of
      Chat cid -> told (Client.Stage cid item) m
      _        -> only m
    reading m = maybe (only m) (\chat -> told (Client.MarkRead chat) m) (openChat m)
    -- QR codes and links have it.
    withoutScheme a
      | Text.toLower (Text.take 4 a) == "tox:" = Text.drop 4 a
      | otherwise = a

-- | Let the client have it, then follow it on screen.
told :: Client.Event -> Model -> (Model, [Command])
told event model = (calling (followed event model model { modelClient = client }), commands)
  where
    (client, commands) = Client.update (viewing model) event (modelClient model)
    -- A new call starts on screen.
    calling m
      | Just c <- clientCall client, Just (key c) /= (key <$> clientCall (modelClient model)) =
          m { modelPhoneCall = phoneCall (callVideo c) }
      | otherwise = m
    key c = (callContact c, callStarted c)

-- | What the screen does when the client has had an event.
followed :: Client.Event -> Model -> Model -> Model
followed event old new = case event of
  Client.ToxRefused refusal -> new { modelNotice = Just (refused refusal) }
  Client.InviteSent {} -> new { modelNotice = Just "Invite sent" }
  Client.Measured report -> new { modelDebug = Just report }
  Client.AddFailed address reason -> new { modelScreen = AddContact, modelAddress = address, modelAddError = Just (notAdded reason) }
  Client.Received cid _ _ _ -> unseen (Chat cid)
  Client.GroupReceived gid _ _ _ _ _ -> unseen (GroupChat gid)
  Client.GroupFileShared gid _ _ _ _ -> unseen (GroupChat gid)
  Client.PrivateReceived gid _ _ _ _ -> unseen (GroupChat gid)
  Client.CallElsewhere _
    | Just c <- clientCall (modelClient old), callPhase c /= Ringing -> new { modelNotice = Just "Another profile has a call" }
  Client.GroupCreated gid _ _ _ -> new { modelScreen = GroupChat gid }
  Client.DeleteContact {} -> new { modelScreen = Contacts }
  Client.DeleteGroup {} -> new { modelScreen = Contacts }
  -- Our own Tox says how the invite went.
  Client.InviteFriend {} | isJust (clientLinked (modelClient new)) -> new { modelNotice = Just "Invite sent" }
  _ -> new
  where
    unseen screen
      | modelScreen new == screen && modelAway new = new { modelUnseen = modelUnseen new + 1 }
      | otherwise = new

refused :: Client.Refusal -> Text
refused = \case
  Client.TopicRefused _                -> "You may not change this group's topic"
  Client.FoundingRefused NewConference -> "Tox couldn't make the conference"
  Client.FoundingRefused _             -> "Tox couldn't make the group"
  Client.InviteRefused {}              -> "Couldn't invite them now"
  Client.ModerationRefused _ _ refusal -> "Tox refused: " <> Text.pack (show refusal)
  Client.SettingRefused _              -> "Tox refused a setting"

notAdded :: Maybe Tox.AddFriendError -> Text
notAdded = \case
  Nothing                 -> "Not a valid Tox ID"
  Just Tox.MessageEmpty   -> "The message can't be empty"
  Just Tox.MessageTooLong -> "The message is too long"
  Just Tox.OwnKey         -> "This is your own Tox ID"
  Just Tox.AlreadyFriend  -> "Already a contact"

viewing :: Model -> Client.Viewing
viewing model = Client.Viewing
  { Client.viewingChat = openChat model
  , Client.viewingShown = modelShown model
  , Client.viewingEnd = not (modelAway model)
  , Client.viewingPage = chatPage
  }

openChat :: Model -> Maybe Client.Chat
openChat model = case modelScreen model of
  Chat cid      -> Just (Client.FriendChat cid)
  GroupChat gid -> Just (Client.GroupChat gid)
  _             -> Nothing

chatScreen :: Client.Chat -> Screen
chatScreen = \case
  Client.FriendChat cid -> Chat cid
  Client.GroupChat gid  -> GroupChat gid

linkError :: Text -> Maybe Text
linkError link
  | Text.null (Text.strip link) = Just "Paste the link the other device shows"
  | isNothing (Pairing.parseLink link) = Just "That isn't a pairing link"
  | otherwise = Nothing

privateGone :: Text
privateGone = "They have left the group, so nothing was sent"

-- | Member the draft goes to privately, while they're in the group.
privateMember :: Model -> Group -> Maybe (Tox.PeerId, Member)
privateMember model g = (`memberByKey` g) =<< modelPrivateTo model

-- | The link a new device pairs with, while its code works.
pairingLink :: Model -> Maybe Text
pairingLink model = (\code -> "tox:" <> profileToxId (clientProfile (modelClient model)) <> "?pair=" <> code)
  <$> (Home.homePairing =<< clientHome (modelClient model))

opened :: Screen -> Model -> Model
opened screen model = case screen of
  InviteScreen _ _ -> model { modelPassword = "" }
  TopicScreen gid -> model { modelTopic = maybe "" groupTopic (Map.lookup gid (clientGroups client)) }
  NewGroupScreen -> model { modelGroupName = "", modelFounding = PrivateGroup }
  GroupSettingsScreen gid -> case groupSettings =<< Map.lookup gid (clientGroups client) of
    Just settings -> model { modelPeerLimit = Text.pack (show (Tox.groupPeerLimit settings))
                           , modelGroupPassword = Text.decodeUtf8Lenient (Tox.groupPassword settings) }
    Nothing -> model
  JoinGroupScreen -> model { modelChatId = "", modelPassword = "" }
  LinkScreen -> model { modelLink = "" }
  UnlockScreen -> model { modelPassword = "" }
  BackupScreen -> model { modelBackup = Set.fromList (map accountId (modelProfiles model)) }
  DebugScreen -> model { modelDebug = Nothing }
  ProfileScreen -> model
    { modelNickname = profileNickname (clientProfile client)
    , modelStatus = profileStatus (clientProfile client)
    }
  ProxyScreen ->
    let proxy = settingProxy (clientDevice client)
    in model
      { modelProxyKind = Tox.proxyType <$> proxy
      , modelProxyHost = maybe "" (Text.pack . Tox.proxyHost) proxy
      , modelProxyPort = maybe "" (Text.pack . show . Tox.proxyPort) proxy
      }
  AddContact -> model
    { modelAddress = ""
    , modelRequest = requestMessage (clientProfile client)
    , modelAddError = Nothing
    }
  _ -> model
  where
    client = modelClient model

-- | What the client holds that may grow, for the debug screen.
clientHolds :: Client -> [(Text, Int)]
clientHolds client =
  [ ("contacts", Map.size (clientContacts client))
  , ("messages", sum (map (length . contactMessages) (Map.elems (clientContacts client))))
  , ("groups", Map.size (clientGroups client))
  , ("group entries", sum (map (length . groupEntries) (Map.elems (clientGroups client))))
  , ("avatars", Map.size (clientAvatars client))
  , ("avatar bytes", sum (map BS.length (Map.elems (clientAvatars client))))
  , ("file transfers", Map.size (clientTransfers client))
  , ("staged", sum (map length (Map.elems (clientStaged client))))
  ]
  ++ maybe [] Linked.linkedHolds (clientLinked client)
  ++ maybe [] Home.homeHolds (clientHome client)

-- | Nothing on the contacts screen (Back leaves the app).
back :: Model -> Maybe Model
back model
  -- Can't hide a ringing call. Others go behind a banner.
  | Just c <- clientCall (modelClient model), not (callHidden (modelPhoneCall model)) =
      if callPhase c == Ringing then Nothing
      else Just (phoned (\p -> p { callHidden = True }) model)
  | modelDrawer model = Just model { modelDrawer = False }
  | modelScreen model == Contacts = Nothing
  | not (Set.null (modelSelected model)) = Just model { modelSelected = Set.empty }
  | Details cid <- modelScreen model = Just model { modelScreen = Chat cid }
  | GroupDetails gid <- modelScreen model = Just model { modelScreen = GroupChat gid }
  | TopicScreen gid <- modelScreen model = Just model { modelScreen = GroupDetails gid }
  | InviteFriendsScreen gid <- modelScreen model = Just model { modelScreen = GroupDetails gid }
  | GroupSettingsScreen gid <- modelScreen model = Just model { modelScreen = GroupDetails gid }
  | Forward chat _ <- modelScreen model = Just model { modelScreen = chatScreen chat }
  | MessageInfo chat _ <- modelScreen model = Just model { modelScreen = chatScreen chat }
  | modelScreen model == ProxyScreen = Just model { modelScreen = SettingsScreen }
  | modelScreen model == LinkScreen = Just model { modelScreen = ProfilesScreen }
  | modelScreen model == UnlockScreen = Just model { modelScreen = ProfilesScreen }
  | modelScreen model == DevicesScreen = Just model { modelScreen = ProfileScreen }
  | modelScreen model == BackupScreen = Just model { modelScreen = ProfilesScreen }
  | otherwise = Just model { modelScreen = Contacts }

-- | Unread messages and files to notify about, oldest first. Not for muted
-- contacts.
notifications :: UTCTime -> Client -> Map ContactId [Text]
notifications now client = Map.mapMaybeWithKey lines' (unreadOf now client)
  where
    lines' cid = nonEmpty . map (preview (named cid)) . filter (isNothing . messageCall)
    named cid = maybe "" (contactLabel cid) (Map.lookup cid (clientContacts client))

callHistory :: Client -> [(ContactId, Message, CallRecord)]
callHistory client = sortOn (\(_, m, _) -> Down (messageTime m))
  [ (cid, m, record)
  | (cid, c) <- Map.toList (clientContacts client), m <- contactMessages c, Just record <- [messageCall m] ]

-- | Unread group messages to notify about, oldest first, with the sender.
-- Muted groups still notify about mentions, unless turned off.
groupNotifications :: UTCTime -> Client -> Map GroupId [Text]
groupNotifications now client = Map.mapMaybe unread (clientGroups client)
  where
    unread g
      | groupUnread g > 0 && (not (groupMuted now g) || groupMentions g) =
          let said = uttered (clientProfile client) g
              received = \case
                GroupEntry _ (Said (Just who) kind text _ _) _ -> [(said who (shownText text), False, kind)]
                GroupEntry _ (PrivateFrom who kind text) _   -> [((memberLabel who, shownText text), True, kind)]
                _                                          -> []
              theirs = concatMap received (groupEntries g)
              -- Private messages count as mentions.
              shown ((_, text), private, _) = not (groupMuted now g) || private || mentions (groupNick (clientProfile client)) text
          in nonEmpty [ line m | m <- drop (length theirs - groupUnread g) theirs, shown m ]
      | otherwise = Nothing
    line ((who, text), private, kind)
      | kind == Tox.MessageAction = "* " <> name <> " " <> text
      | otherwise = name <> ": " <> text
      where name = who <> (if private then " (privately)" else "")

-- | Unread missed calls per contact (notified separately from messages).
-- Not for muted contacts.
missedCalls :: UTCTime -> Client -> Map ContactId Int
missedCalls now = Map.mapMaybe (positive . length . filter (isJust . messageCall)) . unreadOf now
  where positive n = if n > 0 then Just n else Nothing

-- | Theirs, oldest first. Only missed calls count. Nothing for muted
-- contacts.
unreadOf :: UTCTime -> Client -> Map ContactId [Message]
unreadOf now client = Map.mapMaybe unread (clientContacts client)
  where
    unread c
      | contactUnread c > 0 && not (muted now c) =
          let theirs = filter unreadable (contactMessages c)
          in Just (drop (length theirs - contactUnread c) theirs)
      | otherwise = Nothing
    unreadable m = not (messageMine m) && maybe True recordMissed (messageCall m)

nonEmpty :: [a] -> Maybe [a]
nonEmpty xs = if null xs then Nothing else Just xs
