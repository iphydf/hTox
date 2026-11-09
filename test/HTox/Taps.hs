{-# LANGUAGE StrictData #-}
-- The numbers live in a table, which GHC mustn't share or float out.
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
-- | Names for what each tap and edit in a view does. The view only knows them
-- by number, so each name gets the next number the first time it's used.
module HTox.Taps
  ( Tapped (..)
  , handlers
  , tapped
  ) where

import qualified Data.ByteString     as BS
import           Data.Int            (Int32)
import           Data.IORef          (IORef, atomicModifyIORef', newIORef,
                                      readIORef)
import           Data.List           (find)
import           Hatter.Action       (Action (..), OnChange (..), OnScroll (..))
import           HTox.Model          (ContactId, Founding, GroupId, MessageId,
                                      ProfileId)
import           HTox.View           (Handlers (..))
import           System.IO.Unsafe    (unsafePerformIO)
import qualified Tox.Application.Tox as Tox

-- | One for each field of 'Handlers', with what it was given.
data Tapped
  = Back
  | Send
  | OpenDrawer
  | OpenProfile
  | OpenAddContact
  | OpenChats
  | OpenCalls
  | CallBack ContactId
  | AllowFullScreen
  | ApplyProfile
  | Away
  | AddFriend
  | CopyToxId
  | Attach
  | TapMessage ContactId MessageId
  | HoldMessage ContactId MessageId
  | TapEntry GroupId MessageId
  | HoldEntry GroupId MessageId
  | CancelFile ContactId MessageId
  | SendAgain ContactId MessageId
  | GroupFileDownload GroupId BS.ByteString
  | GroupFilePause GroupId BS.ByteString
  | Unselect
  | CopySelected
  | ForwardSelected
  | CancelReply
  | Unstage ContactId Int
  | OpenSettings
  | OpenProfiles
  | SwitchProfile ProfileId
  | DeleteProfile ProfileId
  | NewProfile
  | ImportProfile
  | OpenBackup
  | OpenDebug
  | ExportDebug
  | BackUp
  | ToggleBackup ProfileId
  | Restore
  | PickAvatar
  | RemoveAvatar
  | Export
  | AutoAccept
  | ToggleUdp
  | ToggleIpv6
  | ToggleLan
  | ToggleStay
  | ToggleTyping
  | OpenProxy
  | ProxyKind (Maybe Tox.ProxyType)
  | ProxyHost
  | ProxyPort
  | ApplyProxy
  | ForwardTo ContactId
  | OpenChat ContactId
  | Accept ContactId
  | Reject ContactId
  | Details ContactId
  | MuteChat ContactId
  | Delete ContactId
  | HoldChat ContactId
  | CopyKey ContactId
  | Pictures ContactId
  | Draft
  | Nickname
  | Status
  | Address
  | Request
  | Call Bool
  | Answer Bool
  | HangUp
  | Mute
  | Camera
  | Route
  | FlipCamera
  | HideCall
  | ShowCall
  | ReturnToCall
  | Scrolled
  | Jump
  | OpenGroup GroupId
  | GroupDetails GroupId
  | MuteGroup GroupId
  | GroupPictures GroupId
  | LeaveGroup GroupId
  | DeleteGroup GroupId
  | HoldGroup GroupId
  | CopyChatId GroupId
  | OpenInvite ContactId GroupId
  | Join ContactId GroupId
  | Decline ContactId GroupId
  | Password
  | EditTopic GroupId
  | ToggleMentions GroupId
  | Topic
  | ApplyTopic
  | Founding Founding
  | GroupName
  | Found
  | ChatId
  | JoinPublic
  | InviteFriends GroupId
  | InviteFriend GroupId ContactId
  | Member GroupId Tox.PeerId
  | EndPrivate
  | GroupSettings GroupId
  | Privacy GroupId
  | TopicLock GroupId
  | Voice GroupId
  | RelayBot GroupId
  | StopRelay GroupId
  | PeerLimit
  | GroupPassword
  | ApplyGroupSettings
  | OpenLink
  | LinkEdited
  | Link
  | Unlock
  | OpenDevices
  | PairDevice
  | CopyPairing
  | RemoveDevice ContactId
  deriving (Eq, Show)

handlers :: Handlers
handlers = Handlers
  { hBack = act Back
  , hSend = act Send
  , hOpenDrawer = act OpenDrawer
  , hOpenProfile = act OpenProfile
  , hOpenAddContact = act OpenAddContact
  , hOpenChats = act OpenChats
  , hOpenCalls = act OpenCalls
  , hCallBack = act . CallBack
  , hAllowFullScreen = act AllowFullScreen
  , hApplyProfile = act ApplyProfile
  , hAway = act Away
  , hAddFriend = act AddFriend
  , hCopyToxId = act CopyToxId
  , hAttach = act Attach
  , hTapMessage = \a b -> act (TapMessage a b)
  , hHoldMessage = \a b -> act (HoldMessage a b)
  , hTapEntry = \a b -> act (TapEntry a b)
  , hHoldEntry = \a b -> act (HoldEntry a b)
  , hCancelFile = \a b -> act (CancelFile a b)
  , hSendAgain = \a b -> act (SendAgain a b)
  , hUnselect = act Unselect
  , hCopySelected = act CopySelected
  , hForwardSelected = act ForwardSelected
  , hCancelReply = act CancelReply
  , hUnstage = \a b -> act (Unstage a b)
  , hOpenSettings = act OpenSettings
  , hOpenProfiles = act OpenProfiles
  , hSwitchProfile = act . SwitchProfile
  , hDeleteProfile = act . DeleteProfile
  , hNewProfile = act NewProfile
  , hImportProfile = act ImportProfile
  , hOpenBackup = act OpenBackup
  , hOpenDebug = act OpenDebug
  , hExportDebug = act ExportDebug
  , hBackUp = act BackUp
  , hToggleBackup = act . ToggleBackup
  , hRestore = act Restore
  , hPickAvatar = act PickAvatar
  , hRemoveAvatar = act RemoveAvatar
  , hExport = act Export
  , hAutoAccept = act AutoAccept
  , hToggleUdp = act ToggleUdp
  , hToggleIpv6 = act ToggleIpv6
  , hToggleLan = act ToggleLan
  , hToggleStay = act ToggleStay
  , hToggleTyping = act ToggleTyping
  , hOpenProxy = act OpenProxy
  , hProxyKind = act . ProxyKind
  , hProxyHost = change ProxyHost
  , hProxyPort = change ProxyPort
  , hApplyProxy = act ApplyProxy
  , hForwardTo = act . ForwardTo
  , hOpenChat = act . OpenChat
  , hAccept = act . Accept
  , hReject = act . Reject
  , hDetails = act . Details
  , hMuteChat = act . MuteChat
  , hDelete = act . Delete
  , hHoldChat = act . HoldChat
  , hCopyKey = act . CopyKey
  , hPictures = act . Pictures
  , hDraft = change Draft
  , hNickname = change Nickname
  , hStatus = change Status
  , hAddress = change Address
  , hRequest = change Request
  , hCall = act . Call
  , hAnswer = act . Answer
  , hHangUp = act HangUp
  , hMute = act Mute
  , hCamera = act Camera
  , hRoute = act Route
  , hFlipCamera = act FlipCamera
  , hHideCall = act HideCall
  , hShowCall = act ShowCall
  , hReturnToCall = act ReturnToCall
  , hScrolled = OnScroll (number Scrolled)
  , hJump = act Jump
  , hOpenGroup = act . OpenGroup
  , hGroupDetails = act . GroupDetails
  , hMuteGroup = act . MuteGroup
  , hGroupPictures = act . GroupPictures
  , hLeaveGroup = act . LeaveGroup
  , hDeleteGroup = act . DeleteGroup
  , hHoldGroup = act . HoldGroup
  , hCopyChatId = act . CopyChatId
  , hOpenInvite = \a b -> act (OpenInvite a b)
  , hJoin = \a b -> act (Join a b)
  , hDecline = \a b -> act (Decline a b)
  , hPassword = change Password
  , hEditTopic = act . EditTopic
  , hToggleMentions = act . ToggleMentions
  , hTopic = change Topic
  , hApplyTopic = act ApplyTopic
  , hFounding = act . Founding
  , hGroupName = change GroupName
  , hFound = act Found
  , hChatId = change ChatId
  , hJoinPublic = act JoinPublic
  , hInviteFriends = act . InviteFriends
  , hInviteFriend = \a b -> act (InviteFriend a b)
  , hMember = \a b -> act (Member a b)
  , hGroupFile = \a b -> act (GroupFileDownload a b)
  , hPauseGroupFile = \a b -> act (GroupFilePause a b)
  , hEndPrivate = act EndPrivate
  , hGroupSettings = act . GroupSettings
  , hPrivacy = act . Privacy
  , hTopicLock = act . TopicLock
  , hVoice = act . Voice
  , hRelayBot = act . RelayBot
  , hStopRelay = act . StopRelay
  , hPeerLimit = change PeerLimit
  , hGroupPassword = change GroupPassword
  , hApplyGroupSettings = act ApplyGroupSettings
  , hOpenLink = act OpenLink
  , hLinkEdited = change LinkEdited
  , hLink = act Link
  , hUnlock = act Unlock
  , hOpenDevices = act OpenDevices
  , hPairDevice = act PairDevice
  , hCopyPairing = act CopyPairing
  , hRemoveDevice = act . RemoveDevice
  }

-- | Names with their numbers, newest first.
numbers :: IORef [(Tapped, Int32)]
numbers = unsafePerformIO (newIORef [])
{-# NOINLINE numbers #-}

number :: Tapped -> Int32
number t = unsafePerformIO . atomicModifyIORef' numbers $ \known -> case lookup t known of
  Just n  -> (known, n)
  Nothing -> let n = fromIntegral (length known) in ((t, n) : known, n)
{-# NOINLINE number #-}

act :: Tapped -> Action
act = Action . number

change :: Tapped -> OnChange
change = OnChange . number

-- | What a tap does. The action's number comes first, as it may be new.
tapped :: Action -> Maybe Tapped
tapped (Action n) = n `seq` unsafePerformIO (fmap fst . find ((== n) . snd) <$> readIORef numbers)
{-# NOINLINE tapped #-}
