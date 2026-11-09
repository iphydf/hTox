{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | Widgets of each screen.
module HTox.View
  ( Env (..)
  , Handlers (..)
  , view
  , startFailure
  , peerStream
  , selfStream
  ) where

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as Base16
import           Data.Char                 (toLower)
import           Data.Containers.ListUtils (nubOrdOn)
import           Data.List                 (find, sortOn)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe, isJust, isNothing,
                                            listToMaybe)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Time                 (Day, TimeZone, UTCTime,
                                            defaultTimeLocale, diffDays,
                                            diffUTCTime, formatTime, localDay,
                                            toModifiedJulianDay, utcToLocalTime)
import           Data.Word                 (Word64)
import           Hatter.Action             (Action, OnChange, OnScroll)
import           Hatter.Widget
import           Numeric                   (showFFloat)
import           System.FilePath           (takeExtension)
import qualified Tox.Application.Tox       as Tox
import qualified Tox.Client.Debug          as Debug
import qualified Tox.Client.Home           as Home
import           Tox.Client.Locations      (location)
import           Tox.Client.Pictures       (PictureFormat (..), pictureFormat)
import           Tox.Client.Types          (MessageId (..), entryText,
                                            noticeText, shownText)
import qualified Tox.Client.Update         as Client
import qualified Tox.Core.Lanes            as Lanes
import qualified Tox.Sync.Id               as Sync

import           HTox.Emoji                (emojiOnly)
import           HTox.Model
import           HTox.Theme
import           HTox.Version              (version)

-- | What widgets need besides the model.
data Env = Env
  { envTheme       :: Theme
  , envNow         :: UTCTime
  , envZone        :: TimeZone
  , envAvatar      :: Text -> BS.ByteString
    -- ^ Identicon PNG of a key.
  , envQr          :: BS.ByteString
    -- ^ PNG of our Tox ID.
  , envPictureSize :: FilePath -> Maybe (Int, Int)
  , envDecoded     :: FilePath -> Maybe BS.ByteString
    -- ^ Pictures the platform can't read (e.g. QOI), once decoded.
  , envClock24     :: Bool
  , envElsewhere   :: Maybe Call
    -- ^ Another profile's call.
  , envOthers      :: [(ProfileId, ProfileSummary)]
    -- ^ The other profiles, online too.
  , envSchema      :: Int
    -- ^ The database's version.
  , envGraphics    :: Text
    -- ^ What draws the window, where the platform says.
  }

data Handlers = Handlers
  { hBack               :: Action
  , hSend               :: Action
  , hOpenDrawer         :: Action
  , hOpenProfile        :: Action
  , hOpenAddContact     :: Action
  , hOpenChats          :: Action
  , hOpenCalls          :: Action
  , hCallBack           :: ContactId -> Action
    -- ^ Voice call from the calls list.
  , hAllowFullScreen    :: Action
  , hApplyProfile       :: Action
  , hAway               :: Action
  , hAddFriend          :: Action
  , hCopyToxId          :: Action
  , hAttach             :: Action
  , hTapMessage         :: ContactId -> MessageId -> Action
    -- ^ Opens a file, or accepts one that's waiting.
  , hHoldMessage        :: ContactId -> MessageId -> Action
  , hTapEntry           :: GroupId -> MessageId -> Action
    -- ^ Selects it while others are selected.
  , hHoldEntry          :: GroupId -> MessageId -> Action
  , hCancelFile         :: ContactId -> MessageId -> Action
  , hSendAgain          :: ContactId -> MessageId -> Action
  , hUnselect           :: Action
  , hCopySelected       :: Action
  , hForwardSelected    :: Action
  , hCancelReply        :: Action
  , hUnstage            :: ContactId -> Int -> Action
  , hOpenSettings       :: Action
  , hOpenProfiles       :: Action
  , hSwitchProfile      :: ProfileId -> Action
  , hDeleteProfile      :: ProfileId -> Action
  , hNewProfile         :: Action
  , hImportProfile      :: Action
  , hOpenBackup         :: Action
  , hOpenDebug          :: Action
  , hExportDebug        :: Action
  , hBackUp             :: Action
  , hToggleBackup       :: ProfileId -> Action
  , hRestore            :: Action
  , hPickAvatar         :: Action
  , hRemoveAvatar       :: Action
  , hExport             :: Action
  , hAutoAccept         :: Action
  , hToggleUdp          :: Action
  , hToggleIpv6         :: Action
  , hToggleLan          :: Action
  , hToggleStay         :: Action
  , hToggleTyping       :: Action
  , hOpenProxy          :: Action
  , hProxyKind          :: Maybe Tox.ProxyType -> Action
  , hProxyHost          :: OnChange
  , hProxyPort          :: OnChange
  , hApplyProxy         :: Action
  , hForwardTo          :: ContactId -> Action
  , hOpenChat           :: ContactId -> Action
  , hAccept             :: ContactId -> Action
  , hReject             :: ContactId -> Action
  , hDetails            :: ContactId -> Action
  , hMuteChat           :: ContactId -> Action
  , hDelete             :: ContactId -> Action
  , hHoldChat           :: ContactId -> Action
    -- ^ The chat's menu in the chat list.
  , hCopyKey            :: ContactId -> Action
  , hPictures           :: ContactId -> Action
    -- ^ Offers the picture formats.
  , hDraft              :: OnChange
  , hNickname           :: OnChange
  , hStatus             :: OnChange
  , hAddress            :: OnChange
  , hRequest            :: OnChange
  , hCall               :: Bool -> Action
    -- ^ With or without the camera, like 'hAnswer'.
  , hAnswer             :: Bool -> Action
  , hHangUp             :: Action
  , hMute               :: Action
  , hCamera             :: Action
  , hRoute              :: Action
    -- ^ Switches between earpiece and loudspeaker, or offers all routes if
    -- there are more.
  , hFlipCamera         :: Action
  , hHideCall           :: Action
  , hShowCall           :: Action
  , hReturnToCall       :: Action
    -- ^ To the profile with the call.
  , hScrolled           :: OnScroll
  , hJump               :: Action
  , hOpenGroup          :: GroupId -> Action
  , hGroupDetails       :: GroupId -> Action
  , hMuteGroup          :: GroupId -> Action
  , hGroupPictures      :: GroupId -> Action
  , hLeaveGroup         :: GroupId -> Action
  , hDeleteGroup        :: GroupId -> Action
  , hHoldGroup          :: GroupId -> Action
  , hCopyChatId         :: GroupId -> Action
  , hOpenInvite         :: ContactId -> GroupId -> Action
  , hJoin               :: ContactId -> GroupId -> Action
  , hDecline            :: ContactId -> GroupId -> Action
  , hPassword           :: OnChange
  , hEditTopic          :: GroupId -> Action
  , hToggleMentions     :: GroupId -> Action
  , hTopic              :: OnChange
  , hApplyTopic         :: Action
  , hFounding           :: Founding -> Action
  , hGroupName          :: OnChange
  , hFound              :: Action
  , hChatId             :: OnChange
  , hJoinPublic         :: Action
  , hInviteFriends      :: GroupId -> Action
  , hInviteFriend       :: GroupId -> ContactId -> Action
  , hMember             :: GroupId -> Tox.PeerId -> Action
    -- ^ Opens the member menu.
  , hGroupFile          :: GroupId -> BS.ByteString -> Action
  , hPauseGroupFile     :: GroupId -> BS.ByteString -> Action
    -- ^ Downloads a file shared in the group.
  , hEndPrivate         :: Action
  , hGroupSettings      :: GroupId -> Action
  , hPrivacy            :: GroupId -> Action
  , hTopicLock          :: GroupId -> Action
  , hVoice              :: GroupId -> Action
  , hRelayBot           :: GroupId -> Action
  , hStopRelay          :: GroupId -> Action
  , hPeerLimit          :: OnChange
  , hGroupPassword      :: OnChange
  , hApplyGroupSettings :: Action
  , hOpenLink           :: Action
  , hLinkEdited         :: OnChange
  , hLink               :: Action
  , hUnlock             :: Action
  , hOpenDevices        :: Action
  , hPairDevice         :: Action
  , hRemoveDevice       :: ContactId -> Action
  , hCopyPairing        :: Action
  }

view :: Env -> Handlers -> Model -> Widget
view env h model = case clientCall client of
  Just c | Just contact <- Map.lookup (callContact c) (clientContacts client) ->
    if callHidden (modelPhoneCall model)
      then column [callBanner env (hShowCall h) c, grow (screen env h model)]
      else callScreen env h c (modelPhoneCall model) contact (modelNotice model)
  _ | Just c <- envElsewhere env -> column [callBanner env (hReturnToCall h) c, grow (screen env h model)]
  _ -> screen env h model
  where client = modelClient model

screen :: Env -> Handlers -> Model -> Widget
screen env h model = case modelScreen model of
      Contacts      -> contacts env h model
      Chat cid      -> maybe (contacts env h model) (chat env h model cid)
                         (Map.lookup cid (clientContacts client))
      Details cid   -> maybe (contacts env h model) (details env h cid)
                         (Map.lookup cid (clientContacts client))
      Forward _ _   -> forward env h model
      MessageInfo (Client.FriendChat cid) i
        | Just contact <- Map.lookup cid (clientContacts client)
        , Just m <- find ((== i) . messageId) (contactMessages contact) -> messageInfo env h cid contact m
      MessageInfo (Client.GroupChat gid) i
        | Just g <- Map.lookup gid (clientGroups client)
        , Just e <- find ((== i) . entryId) (groupEntries g) -> groupMessageInfo env h model g e
      MessageInfo _ _ -> contacts env h model
      CallsScreen   -> calls env h model
      ProfileScreen -> profile env h model
      SettingsScreen -> settings env h model
      ProfilesScreen -> profiles env h model
      ProxyScreen   -> proxy env h model
      AddContact    -> addContact env h model
      GroupChat gid -> maybe (contacts env h model) (groupChat env h model gid)
                         (Map.lookup gid (clientGroups client))
      GroupDetails gid -> maybe (contacts env h model) (groupDetails env h model gid)
                         (Map.lookup gid (clientGroups client))
      InviteScreen cid gid
        | Just invite <- Map.lookup (cid, gid) (clientInvites client) -> inviteScreen env h model cid gid invite
        | otherwise -> contacts env h model
      TopicScreen gid -> maybe (contacts env h model) (topicScreen env h model) (Map.lookup gid (clientGroups client))
      NewGroupScreen -> founding env h model
      GroupSettingsScreen gid -> maybe (contacts env h model) (groupSettingsScreen env h model gid) (Map.lookup gid (clientGroups client))
      JoinGroupScreen -> joinGroup env h model
      InviteFriendsScreen gid -> maybe (contacts env h model) (inviteFriends env h model gid) (Map.lookup gid (clientGroups client))
      LinkScreen -> linking env h model
      UnlockScreen -> unlocking env h model
      DevicesScreen -> devicesScreen env h model
      BackupScreen -> backup env h model
      DebugScreen -> debugScreen env h model
  where client = modelClient model

-- | Chats, with invites and friend requests on top.
contacts :: Env -> Handlers -> Model -> Widget
contacts env h model = stack $
  column
      [ appBar t
          [ iconButton t (hOpenDrawer h) "ic_menu"
          , gap t 20
          , icon (if clientConnection client == Offline then "ic_offline_bolt" else "ic_online_prediction")
          , gap t 16
          , grow (label 22 (pText p) ("hTox (" <> profileNickname (clientProfile client) <> ")"))
          ]
      , grow $ stack
          [ if null (chatOrder client) && Map.null (clientRequests client) && Map.null (clientInvites client)
              then stack [empty]
              else scrollColumn $ map inviteRow (Map.toList (clientInvites client))
                ++ map requestRow (Map.toList (clientRequests client))
                ++ map chatRow (chatOrder client)
          , Tappable (hOpenAddContact h) $ styled
              (\s -> s { wsBackgroundColor = Just (pMine p), wsCornerRadius = Just (dp t 16)
                       , wsPadding = Just (dp t 18), wsMargin = Just (dp t 16)
                       , wsAlignSelf = Just SelfEnd })
              (icon "ic_add")
          ]
      , tabs env h Contacts
      ]
  : [ drawer | modelDrawer model ]
  where
    client = modelClient model
    t = envTheme env
    p = thPalette t
    empty = middle $ padded t 16 $ column
      [ middle $ styled (\s -> s { wsWidth = Just (dp t 48), wsHeight = Just (dp t 48) })
          (icon "ic_chat_bubble_outline")
      , gap t 16
      , middle (label 16 (pText p) "No chats yet")
      , gap t 4
      , middle (label 14 (pMuted p) "Add a contact with the + button to start chatting")
      ]
    requestRow (cid@(ContactId key), message) = padded t 14 $ row
      [ middle (avatar env 48 key Nothing)
      , gap t 16
      , grow $ middle $ column
          [ maxLines 2 (label 16 (pText p) message)
          , maxLines 1 (label 14 (pMuted p) key)
          ]
      , middle (iconButton t (hAccept h cid) "ic_check")
      , middle (iconButton t (hReject h cid) "ic_close")
      ]
    inviteRow ((cid, gid@(GroupId chatKey)), invite) = Tappable (hOpenInvite h cid gid) $ padded t 14 $ row
      [ middle (avatar env 48 chatKey Nothing)
      , gap t 16
      , grow $ middle $ column
          [ maxLines 1 (labelIn bold 16 (pText p) (inviteLabel invite))
          , maxLines 2 $ label 14 (pMuted p) $ case inviteAnswered invite of
              Just _  -> "Joins when " <> friendName cid <> " is online"
              Nothing -> friendName cid <> " invited you to " <> article (kindName (inviteKind invite))
          ]
      , middle (iconButton t (hDecline h cid gid) "ic_close")
      ]
    friendName cid = maybe "A friend" (contactLabel cid) (Map.lookup cid (clientContacts client))
    chatRow = \case
      FriendRow cid contact -> contactRow (cid, contact)
      GroupRow gid g -> groupRow gid g
    groupRow gid@(GroupId chatKey) g = Tappable (hOpenGroup h gid) $ LongPressable (hHoldGroup h gid) $ padded t 14 $ row
      [ middle (avatar env 48 chatKey Nothing)
      , gap t 16
      , grow $ middle $ column
          [ row [ middle (styled (\s -> s { wsWidth = Just (dp t 16), wsHeight = Just (dp t 16) }) (icon "ic_group"))
                , gap t 6
                , grow (middle (labelIn (if groupUnread g > 0 then bold else id) 16 (pText p) (groupLabel g))) ]
          , case Map.lookup gid (clientGroupDrafts client) of
              Just draft | not (Text.null (Text.strip draft)) -> row
                [label 14 (pError p) "Draft: ", grow (maxLines 1 (label 14 (pMuted p) draft))]
              _ -> maxLines 1 (label 14 (pMuted p) (maybe (phaseText (groupPhase g)) (entryPreview (uttered (clientProfile client) g)) (lastEntry g)))
          ]
      , middle $ column $
          styled (\s -> s { wsAlignSelf = Just SelfEnd })
            (label 12 (pMuted p) (maybe "" (listTime env . entryTime) (lastEntry g)))
          : [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsWidth = Just (dp t 16), wsHeight = Just (dp t 16) })
                (icon "ic_notifications_off")
            | groupMuted (envNow env) g ]
          ++ [ styled (\s -> s { wsAlignSelf = Just SelfEnd
                            , wsBackgroundColor = Just (if groupMuted (envNow env) g then pMuted p else pAccent p)
                            , wsCornerRadius = Just (dp t 10), wsPadding = Just (dp t 4)
                            , wsMargin = Just (dp t 2) }) $
              label 12 (pOnAccent p) (Text.pack (show (groupUnread g)))
          | groupUnread g > 0 ]
      ]
    contactRow (cid@(ContactId key), contact) = Tappable (hOpenChat h cid) $ LongPressable (hHoldChat h cid) $ padded t 14 $ row
      [ middle (avatar env 48 key (Just (contactOnline contact)))
      , gap t 16
      , grow $ middle $ column
          [ labelIn (if contactUnread contact > 0 then bold else id) 16 (pText p) (contactLabel cid contact)
          , case Map.lookup cid (clientDrafts client) of
              _ | Set.member cid (clientTyping client) -> label 14 (pAccent p) "typing\x2026"
              Just draft | not (Text.null (Text.strip draft)) -> row
                [label 14 (pError p) "Draft: ", grow (maxLines 1 (label 14 (pMuted p) draft))]
              _ -> maxLines 1 (label 14 (pMuted p) (maybe (waiting contact) (preview (contactLabel cid contact)) (lastMessage contact)))
          ]
      , middle $ column $
          styled (\s -> s { wsAlignSelf = Just SelfEnd })
            (label 12 (pMuted p) (maybe "" (listTime env . messageTime) (lastMessage contact)))
          : [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsWidth = Just (dp t 16), wsHeight = Just (dp t 16) })
                (icon "ic_notifications_off")
            | muted (envNow env) contact ]
          ++ [ styled (\s -> s { wsAlignSelf = Just SelfEnd
                            , wsBackgroundColor = Just (if muted (envNow env) contact then pMuted p else pAccent p)
                            , wsCornerRadius = Just (dp t 10), wsPadding = Just (dp t 4)
                            , wsMargin = Just (dp t 2) }) $
              label 12 (pOnAccent p) (Text.pack (show (contactUnread contact)))
          | contactUnread contact > 0 ]
      ]
    waiting contact = maybe "" (const "Friend request sent") (contactRequest contact)
    drawer = stack
      [ Tappable (hBack h) $ styled
          (\s -> s { wsBackgroundColor = Just (pScrim p), wsAlignSelf = Just SelfStretch })
          (text "")
      , styled (\s -> s { wsBackgroundColor = Just (pBar p), wsAlignSelf = Just SelfStretch
                        , wsWidth = Just (thScreenWidth t * 0.8) }) $ column $
          [ Tappable (hOpenProfile h) $ padded t 24 $ row
              [ grow $ middle $ column
                  [ label 24 (pText p) (profileNickname (clientProfile client))
                  , label 14 (pMuted p) (profileStatus (clientProfile client))
                  ]
              , middle (avatar env 48 (ownKey model) Nothing)
              ]
          , divider t
          ]
          ++ map other (envOthers env)
          ++ [ divider t | not (null (envOthers env)) ]
          ++
          [ Tappable (hOpenProfiles h) $ padded t 24 $ label 16 (pText p) "Profiles"
          , Tappable (hOpenSettings h) $ padded t 24 $ label 16 (pText p) "Settings"
          , Tappable (hOpenDebug h) $ padded t 24 $ label 16 (pText p) "Debug"
          ]
      ]
    -- A tap switches to it.
    other (pid, summary) = Tappable (hSwitchProfile h pid) $ padded t 16 $ row $
      [ middle (avatar env 32 (summaryKey summary) Nothing)
      , gap t 16
      , grow $ middle $ column
          [ label 16 (pText p) (summaryName summary)
          , label 14 (pMuted p) (connectionText (summaryConnection summary))
          ]
      ]
      ++ [ middle $ styled (\s -> s { wsBackgroundColor = Just (pAccent p), wsCornerRadius = Just (dp t 10)
                                    , wsPadding = Just (dp t 4) }) $
             label 12 (pOnAccent p) (Text.pack (show (summaryUnread summary)))
         | summaryUnread summary > 0 ]

-- | At the bottom of the screen.
tabs :: Env -> Handlers -> Screen -> Widget
tabs env h current = styled (\s -> s { wsBackgroundColor = Just (pBar p) }) $ row
  [ tab Contacts (hOpenChats h) "ic_chat_bubble_outline" "Chats"
  , tab CallsScreen (hOpenCalls h) "ic_call" "Calls"
  ]
  where
    t = envTheme env
    p = thPalette t
    tab screen' action name caption = grow $ Tappable action $ padded t 8 $ column
      [ middle $ styled (\s -> s { wsWidth = Just (dp t 24), wsHeight = Just (dp t 24) }) (icon name)
      , middle (label 12 (if screen' == current then pAccent p else pMuted p) caption)
      ]

-- | Tap one to call back.
calls :: Env -> Handlers -> Model -> Widget
calls env h model = column $
  [ appBar t [gap t 16, grow (label 22 (pText p) "Calls")] ]
  ++ [ styled (\s -> s { wsBackgroundColor = Just (pTheirs p) }) $ padded t 12 $ row
         [ grow $ middle $ label 14 (pText p) "Calls can't ring over the lock screen"
         , middle (pill t (pMine p) (hAllowFullScreen h) "Allow")
         ]
     | not (modelFullScreen model) ]
  ++
  [ grow $ case [ (cid, c, m, record) | (cid, m, record) <- callHistory client
                                      , Just c <- [Map.lookup cid (clientContacts client)] ] of
      []      -> stack [middle (label 16 (pMuted p) "No calls yet")]
      history -> scrollColumn (map callRow history)
  , tabs env h CallsScreen
  ]
  where
    client = modelClient model
    t = envTheme env
    p = thPalette t
    callRow (cid@(ContactId key), contact, m, record) = Tappable (hOpenChat h cid) $ padded t 14 $ row $
      [ middle (avatar env 48 key (Just (contactOnline contact)))
      , gap t 16
      , grow $ middle $ column
          [ label 16 (if recordMissed record then pError p else pText p) (contactLabel cid contact)
          , row
              [ middle $ styled (\s -> s { wsWidth = Just (dp t 16), wsHeight = Just (dp t 16) })
                  (icon (callIcon (messageMine m) record))
              , gap t 4
              , label 14 (pMuted p) $ listTime env (messageTime m)
                  <> maybe "" (\talked -> " \x00B7 " <> duration (round talked)) (recordDuration record)
              ]
          ]
      ]
      ++ [ middle (iconButton t (hCallBack h cid) "ic_call") | contactOnline contact ]

callIcon :: Bool -> CallRecord -> Text
callIcon mine record
  | recordMissed record = "ic_call_missed"
  | mine = "ic_call_made"
  | otherwise = "ic_call_received"

chat :: Env -> Handlers -> Model -> ContactId -> Contact -> Widget
chat env h model cid@(ContactId key) contact = column $
  [ if not (Set.null (modelSelected model))
      then selectionBar env h model
      else styled (\s -> s { wsBackgroundColor = Just (pBar p) }) $ appBar t $
      [ iconButton t (hBack h) "ic_arrow_back"
      , grow $ Tappable (hDetails h cid) $ row
          [ middle (avatar env 40 key (Just (contactOnline contact)))
          , gap t 16
          , grow $ middle $ column $
              label 22 (pText p) (contactLabel cid contact)
              : [label 14 (pAccent p) "typing\x2026" | Set.member cid (clientTyping (modelClient model))]
          ]
      ]
      ++ concat [ [iconButton t (hCall h True) "ic_videocam", iconButton t (hCall h False) "ic_call"]
                | contactOnline contact ]
  , grow $ stack $
      Scrolled (hScrolled h) (modelJump model)
        (styled (\s -> s { wsPadding = Just (dp t 10), wsKeepAtEnd = Just True })
          (LazyColumn (messages Nothing (contactMessages contact))))
      -- Jump to the newest, with how many came in meanwhile.
      : [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsMargin = Just (dp t 12) }) $ column
            [ grow (text "")
            , Tappable (hJump h) $ styled
                (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 24)
                         , wsPadding = Just (dp t 12) }) $
                label 16 (pText p) (if modelUnseen model > 0 then "\x2193 " <> Text.pack (show (modelUnseen model)) else "\x2193")
            ]
        | modelAway model ]
  ]
  ++ [ styled (\s -> s { wsAlignSelf = Just SelfCenter, wsPadding = Just (dp t 8) }) (label 14 (pError p) notice)
     | Just notice <- [modelNotice model] ]
  ++ [ styled (\s -> s { wsPadding = Just (dp t 8) }) $ scrollRow (zipWith tile [0 ..] staged) | not (null staged) ]
  ++ [ replyBar env h quote | Just quote <- [replied client (Client.FriendChat cid)] ]
  ++
  [ row
      [ grow $ middle $ styled
          (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 28)
                   , wsPadding = Just (dp t 16), wsMargin = Just (dp t 8) })
          (maxLines 3 (input InputMultiline (hDraft h) "Tox message" draft))
      , middle $ Tappable (if writing then hSend h else hAttach h) $ styled
          (\s -> s { wsBackgroundColor = Just (pMine p), wsCornerRadius = Just (dp t 26)
                   , wsPadding = Just (dp t 14), wsMargin = Just (dp t 8) })
          (icon (if writing then "ic_send" else "ic_attach_file"))
      ]
  ]
  where
    client = modelClient model
    t = envTheme env
    p = thPalette t
    draft = Map.findWithDefault "" cid (clientDrafts client)
    staged = Map.findWithDefault [] cid (clientStaged client)
    writing = not (Text.null (Text.strip draft)) || not (null staged)
    tile k picked = styled (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 12)
                                  , wsMargin = Just (dp t 4), wsPadding = Just (dp t 4) }) $ column
      [ styled (\s -> s { wsAlignSelf = Just SelfEnd }) (iconButton t (hUnstage h cid k) "ic_close")
      , case picked of
          StagedFile path name _ _
            | isPicture path || isJust (envDecoded env path) ->
                styled (\s -> s { wsWidth = Just (dp t 96), wsHeight = Just (dp t 96), wsCornerRadius = Just (dp t 8) }) $
                  Image ImageConfig
                    { icSource = maybe (ImageFile path) ImageData (envDecoded env path), icScaleType = ScaleFill }
            | otherwise -> styled (\s -> s { wsWidth = Just (dp t 96) }) $ column
                [ middle $ styled (\s -> s { wsWidth = Just (dp t 36), wsHeight = Just (dp t 36) }) (icon "ic_insert_drive_file")
                , maxLines 2 (label 12 (pText p) name) ]
          StagedPlace latitude longitude -> styled (\s -> s { wsWidth = Just (dp t 96) }) $ column
            [ label 28 (pText p) "\x1F4CD"
            , label 12 (pMuted p) (Text.pack (showFFloat (Just 3) latitude "" <> ", " <> showFFloat (Just 3) longitude "")) ]
      ]
    messages _ [] = []
    messages previous (m : ms) =
      [ keyedItem (dayKey (day m)) $ styled (\s -> s { wsAlignSelf = Just SelfCenter, wsPadding = Just (dp t 8) }) $
          label 14 (pMuted p) (dayHeading env (messageTime m))
      | fmap day previous /= Just (day m) ]
      ++ keyedItem (messageKey (messageId m))
           (held (messageId m) (spaced t (maybe True (\b -> not (together b m)) previous)
             (bubble (messageId m) m (maybe True (not . together m) (listToMaybe ms)))))
      : messages (Just m) ms
    held i = selectable env model (hTapMessage h cid i) (hHoldMessage h cid i) i
    day = localDay . utcToLocalTime (envZone env) . messageTime
    -- Messages from the same side close together form a run, which only
    -- shows the time of its last message.
    together m n = messageMine m == messageMine n && day m == day n
      && diffUTCTime (messageTime n) (messageTime m) < 180
      && all (\x -> isNothing (messageCall x) && isNothing (messageFile x)) [m, n]
    box m = styled
      (\s -> s { wsBackgroundColor = Just (if messageMine m then pMine p else pTheirs p)
               , wsCornerRadius = Just (dp t 20), wsPadding = Just (dp t 12)
               , wsMargin = Just (dp t bubbleMargin)
               , wsAlignSelf = Just (if messageMine m then SelfEnd else SelfStart) })
    bubble i m lastOfRun = case (messageCall m, messageFile m) of
      (Just record, _) -> styled
        (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 16)
                 , wsPadding = Just (dp t 8), wsMargin = Just (dp t 8), wsAlignSelf = Just SelfCenter }) $ row
        [ middle $ styled (\s -> s { wsWidth = Just (dp t 16), wsHeight = Just (dp t 16) })
            (icon (callIcon (messageMine m) record))
        , gap t 8
        , middle (label 13 (if recordMissed record then pError p else pText p) (preview "" m))
        , gap t 8
        , middle (stamp m)
        ]
      (_, Nothing)
        | messageKind m == Tox.MessageNormal, emojiOnly (shownText (messageText m)) ->
            styled (\s -> s { wsMargin = Just (dp t bubbleMargin), wsAlignSelf = Just (if messageMine m then SelfEnd else SelfStart) }) $
              column $ label 48 (pText p) (shownText (messageText m))
                : [ styled (\s -> s { wsAlignSelf = Just (if messageMine m then SelfEnd else SelfStart) }) (stamp m) | lastOfRun ]
      (_, Nothing) -> box m $ row $
        [ styled (\s -> s { wsMaxWidth = Just (thScreenWidth t * 0.6) }) $ case location (shownText (messageText m)) of
            -- A tap opens the maps app.
            Just (latitude, longitude) -> column
              [ label 16 (pText p) (preview (contactLabel cid contact) m)
              , label 13 (pMuted p) (Text.pack (showFFloat (Just 5) latitude "" <> ", " <> showFFloat (Just 5) longitude ""))
              ]
            Nothing -> label 16 (pText p) (preview (contactLabel cid contact) m)
        ]
        ++ concat [ [gap t 8, stamp m] | lastOfRun ]
        -- Within a run, only show that it's pending.
        ++ concat [ [gap t 8, styled (\s -> s { wsAlignSelf = Just SelfEnd }) (deliveryMark Pending)]
                  | not lastOfRun, messageMine m, messageDelivery m == Pending ]
      (_, Just file)
        | isPicture (attachmentPath file) || isJust (envDecoded env (attachmentPath file))
        , messageMine m || attachmentState file == Complete ->
            box m $ column $
              [ styled (\s -> s { wsWidth = Just pictureWidth, wsHeight = Just (pictureHeight (attachmentPath file))
                                , wsCornerRadius = Just (dp t 12) }) $
                  Image ImageConfig
                    { icSource = maybe (ImageFile (attachmentPath file)) ImageData (envDecoded env (attachmentPath file))
                    , icScaleType = ScaleFill }
              , gap t 4
              ]
              ++ [ row (middle (label 13 (pMuted p) (fileStatus True file)) : stopping i m file)
                 | attachmentState file /= Complete ]
              ++ again i m file
              ++ [stamp m]
        | otherwise -> box m $ row $
            [ middle $ styled (\s -> s { wsWidth = Just (dp t 36), wsHeight = Just (dp t 36) })
                (icon "ic_insert_drive_file")
            , gap t 8
            , middle $ styled (\s -> s { wsMaxWidth = Just (thScreenWidth t * 0.5) }) $ column $
                [ maxLines 2 (label 16 (pText p) (shownText (messageText m)))
                , label 13 (pMuted p) (fileStatus (messageMine m) file)
                ]
                ++ [ row [pill t (pMine p) (hTapMessage h cid i) "Accept", gap t 8, pill t (pTheirs p) (hCancelFile h cid i) "Decline"]
                   | not (messageMine m), attachmentState file == Waiting ]
                ++ again i m file
            ]
            ++ stopping i m file
            ++ [gap t 8, stamp m]
    stopping i m file =
      [ middle (iconButton t (hCancelFile h cid i) "ic_close")
      | attachmentState file == Moving || (messageMine m && attachmentState file == Waiting) ]
    -- A linked profile's home device has the file.
    again i m file =
      [ row [pill t (pMine p) (hSendAgain h cid i) "Send again"]
      | messageMine m, attachmentState file == Cancelled, isNothing (clientLinked (modelClient model)) ]
    pictureWidth = thScreenWidth t * 0.6
    -- Height for that width, up to 4:3 upright.
    pictureHeight path = case envPictureSize env path of
      Just (across, down) | across > 0 -> min (pictureWidth * 4 / 3) (pictureWidth * fromIntegral down / fromIntegral across)
      _ -> pictureWidth * 3 / 4
    stamp m = styled (\s -> s { wsAlignSelf = Just SelfEnd }) $ row $
      label 12 (pMuted p) (localTime env (clockFormat env) (messageTime m))
        : concat
            [ [gap t 3, deliveryMark (messageDelivery m)]
            | messageMine m, Nothing <- [messageFile m], Nothing <- [messageCall m] ]
    deliveryMark delivery = styled (\s -> s { wsWidth = Just (dp t 14), wsHeight = Just (dp t 14) })
      (icon (deliveryIcon delivery))
    deliveryIcon delivery = case delivery of
      Pending   -> "ic_schedule"
      Sent      -> "ic_done"
      Delivered -> "ic_done_all"

-- | The bar over a chat while messages in it are selected.
selectionBar :: Env -> Handlers -> Model -> Widget
selectionBar env h model = styled (\s -> s { wsBackgroundColor = Just (pBar p) }) $ appBar t
  [ iconButton t (hUnselect h) "ic_close"
  , gap t 8
  , grow (middle (label 22 (pText p) (Text.pack (show (Set.size (modelSelected model))) <> " selected")))
  , iconButton t (hCopySelected h) "ic_content_copy"
  , iconButton t (hForwardSelected h) "ic_forward"
  ]
  where
    t = envTheme env
    p = thPalette t

-- | Space around each bubble. Those of a run sit this close, and 'runGap'
-- sets runs apart.
bubbleMargin, runGap :: Double
bubbleMargin = 1
runGap = 6

-- | With the gap above it where it starts a run.
spaced :: Theme -> Bool -> Widget -> Widget
spaced t first b
  | first = styled (\s -> s { wsAlignSelf = Just SelfStretch }) (column [gap t runGap, b])
  | otherwise = b

-- | A tap selects while others are selected, a long press opens its menu.
selectable :: Env -> Model -> Action -> Action -> MessageId -> Widget -> Widget
selectable env model tap hold i b = Tappable tap $ LongPressable hold $
  if Set.member i (modelSelected model)
    then styled (\s -> s { wsBackgroundColor = Just (pDivider p), wsAlignSelf = Just SelfStretch }) (column [b])
    else b
  where p = thPalette (envTheme env)

-- | Over the input: what the draft replies to.
replyBar :: Env -> Handlers -> Text -> Widget
replyBar env h quote = styled (\s -> s { wsBackgroundColor = Just (pTheirs p), wsPadding = Just (dp t 8)
                                       , wsMargin = Just (dp t 8), wsCornerRadius = Just (dp t 12) }) $ row
  [ middle (icon "ic_reply")
  , gap t 8
  , grow $ middle $ maxLines 3 $ label 14 (pMuted p) quote
  , middle (iconButton t (hCancelReply h) "ic_close")
  ]
  where
    t = envTheme env
    p = thPalette t

-- | Video streams of the friend and us.
peerStream, selfStream :: Text
peerStream = "peer"
selfStream = "self"

lastEntry :: Group -> Maybe GroupEntry
lastEntry g = case reverse (groupEntries g) of
  e : _ -> Just e
  []    -> Nothing

-- | For the chat list, with the sender from 'uttered'.
entryPreview :: (Member -> Text -> (Text, Text)) -> GroupEntry -> Text
entryPreview utter (GroupEntry _ what _) = case what of
  Said who kind body _ _
    | kind == Tox.MessageAction -> "* " <> by <> " " <> said
    | otherwise -> by <> ": " <> said
    where (by, said) = maybe ("You", shownText body) (`utter` shownText body) who
  SharedFile who file _ -> maybe "You" memberLabel who <> " shared " <> fromMaybe "a file" (groupFileName file)
  PrivateFrom who _ said -> memberLabel who <> " (privately): " <> shownText said
  PrivateTo who _ said _ -> "You to " <> memberLabel who <> " (privately): " <> shownText said
  other -> fromMaybe "" (noticeText other)

-- | Joins and leaves in a row show as one line from this many on, as
-- members on phones come and go all day.
manyComings :: Int
manyComings = 3

-- | Who joined and who left, each once: "Alice, Bob and 3 others joined ·
-- Carol left".
comings :: [Member] -> [Member] -> Text
comings came went = Text.intercalate " \x00B7 " $
  [names came <> " joined" | not (null came)] ++ [names went <> " left" | not (null went)]
  where
    names ms = case map memberLabel (nubOrdOn memberKey ms) of
      [a]          -> a
      [a, b]       -> a <> " and " <> b
      [a, b, c]    -> a <> ", " <> b <> " and " <> c
      a : b : more -> a <> ", " <> b <> " and " <> Text.pack (show (length more)) <> " others"
      []           -> ""

phaseText :: GroupPhase -> Text
phaseText = \case
  Joining                         -> "Joining\x2026"
  InGroup                         -> ""
  Refused Tox.JoinFailFull        -> "Couldn't join: the group is full"
  Refused Tox.JoinFailBadPassword -> "Couldn't join: the password was wrong. Ask for a new invite."
  Refused Tox.JoinFailUnknown     -> "Couldn't join. Ask for a new invite."
  Gone                            -> "You left this group"
  Kicked                          -> "You were removed from this group"

-- | Shown under a name.
statusName :: Tox.UserStatus -> Maybe Text
statusName = \case
  Tox.Away   -> Just "Away"
  Tox.Busy   -> Just "Busy"
  Tox.Online -> Nothing

roleName :: Tox.Role -> Maybe Text
roleName = \case
  Tox.Founder   -> Just "Founder"
  Tox.Moderator -> Just "Moderator"
  Tox.Observer  -> Just "Observer"
  Tox.User      -> Nothing

groupChat :: Env -> Handlers -> Model -> GroupId -> Group -> Widget
groupChat env h model gid@(GroupId chatKey) g = column $
  [ if not (Set.null (modelSelected model)) then selectionBar env h model else
    styled (\s -> s { wsBackgroundColor = Just (pBar p) }) $ appBar t
      [ iconButton t (hBack h) "ic_arrow_back"
      , grow $ Tappable (hGroupDetails h gid) $ row
          [ middle (avatar env 40 chatKey Nothing)
          , gap t 16
          , grow $ middle $ column
              [ maxLines 1 (label 22 (pText p) (groupLabel g))
              , maxLines 1 (label 13 (pMuted p) subtitle)
              ]
          ]
      ]
  , grow $ stack $
      Scrolled (hScrolled h) (modelJump model)
        (styled (\s -> s { wsPadding = Just (dp t 10), wsKeepAtEnd = Just True })
          (LazyColumn (entries Nothing (groupEntries g))))
      : [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsMargin = Just (dp t 12) }) $ column
            [ grow (text "")
            , Tappable (hJump h) $ styled
                (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 24)
                         , wsPadding = Just (dp t 12) }) $
                label 16 (pText p) (if modelUnseen model > 0 then "\x2193 " <> Text.pack (show (modelUnseen model)) else "\x2193")
            ]
        | modelAway model ]
  ]
  ++ [ styled (\s -> s { wsAlignSelf = Just SelfCenter, wsPadding = Just (dp t 12) }) $
         label 14 (if groupPhase g == Joining then pMuted p else pError p) (phaseText (groupPhase g))
     | groupPhase g /= InGroup ]
  ++ [ replyBar env h quote | groupPhase g `elem` [Joining, InGroup], Just quote <- [replied client (Client.GroupChat gid)] ]
  ++ [ styled (\s -> s { wsBackgroundColor = Just (pTheirs p), wsPadding = Just (dp t 8)
                       , wsMargin = Just (dp t 8), wsCornerRadius = Just (dp t 12) }) $ row
         [ middle (icon "ic_reply")
         , gap t 8
         , grow $ middle $ label 14 (pError p) ("Privately to " <> memberLabel m)
         , middle (iconButton t (hEndPrivate h) "ic_close")
         ]
     | groupPhase g == InGroup, Just (_, m) <- [privateMember model g] ]
  ++
  [ row
      [ grow $ middle $ styled
          (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 28)
                   , wsPadding = Just (dp t 16), wsMargin = Just (dp t 8) })
          (maxLines 3 (input InputMultiline (hDraft h) "Message" (Map.findWithDefault "" gid (clientGroupDrafts client))))
      , middle $ Tappable (if writing then hSend h else hAttach h) $ styled
          (\s -> s { wsBackgroundColor = Just (pMine p), wsCornerRadius = Just (dp t 26)
                   , wsPadding = Just (dp t 14), wsMargin = Just (dp t 8) })
          (icon (if writing then "ic_send" else "ic_attach_file"))
      ]
  | groupPhase g `elem` [Joining, InGroup] ]
  where
    client = modelClient model
    t = envTheme env
    p = thPalette t
    -- Files go only once we're in.
    writing = not (Text.null (Text.strip (Map.findWithDefault "" gid (clientGroupDrafts client)))) || groupPhase g /= InGroup
    subtitle = case groupPhase g of
      InGroup -> let n = Map.size (groupMembers g) + 1
                 in memberCount n
      phase -> phaseText phase
    day = localDay . utcToLocalTime (envZone env) . entryTime
    entries _ [] = []
    entries before (e : es) =
      [ keyedItem (dayKey (day e)) $ styled (\s -> s { wsAlignSelf = Just SelfCenter, wsPadding = Just (dp t 8) }) $
          label 14 (pMuted p) (dayHeading env (entryTime e))
      | fmap day before /= Just (day e) ]
      ++ case span (\x -> comingOrGoing x && day x == day e) (e : es) of
        (run, rest) | length run >= manyComings ->
          keyedItem (messageKey (entryId e)) (styled (\s -> s { wsAlignSelf = Just SelfCenter, wsPadding = Just (dp t 4) }) $
            label 13 (pMuted p) (comings [m | Arrived m <- map entryWhat run] [m | Departed m <- map entryWhat run]))
          : entries (listToMaybe (reverse run)) rest
        _ -> let first = maybe True (\b -> not (together b e)) before
             in keyedItem (messageKey (entryId e)) (held e (spaced t (first && bubbled e) (entry first e (maybe True (not . together e) (listToMaybe es)))))
          : entries (Just e) es
    bubbled x = case entryWhat x of
      Said {}        -> True
      SharedFile {}  -> True
      PrivateFrom {} -> True
      PrivateTo {}   -> True
      _              -> False
    -- Only what members wrote.
    held e
      | isJust (entryText e) = selectable env model (hTapEntry h gid (entryId e)) (hHoldEntry h gid (entryId e)) (entryId e)
      | otherwise = id
    comingOrGoing x = case entryWhat x of
      Arrived _  -> True
      Departed _ -> True
      _          -> False
    -- Sender's name above the first message of their run.
    entry first this lastOfRun = case entryWhat this of
          SharedFile who file _ ->
            let mine = isNothing who
                name = fromMaybe "A file" (groupFileName file)
                bubble = styled
                  (\s -> s { wsBackgroundColor = Just (if mine then pMine p else pTheirs p)
                           , wsCornerRadius = Just (dp t 20), wsPadding = Just (dp t 12), wsMargin = Just (dp t bubbleMargin)
                           , wsAlignSelf = Just (if mine then SelfEnd else SelfStart) }) . column
                heading = [ labelIn bold 13 (pAccent p) (maybe "You" memberLabel who) | first, not mine ]
            in bubble $ heading ++ case groupFileData file of
              Just a | attachmentState a == Complete, isPicture (attachmentPath a) || isJust (envDecoded env (attachmentPath a)) ->
                [ styled (\s -> s { wsWidth = Just pictureWidth, wsHeight = Just (pictureHeight (attachmentPath a)), wsCornerRadius = Just (dp t 12) }) $
                    Image ImageConfig
                      { icSource = maybe (ImageFile (attachmentPath a)) ImageData (envDecoded env (attachmentPath a))
                      , icScaleType = ScaleFill }
                , stamp mine Delivered (entryTime this) ]
              attachment ->
                [ row $
                    [ middle $ styled (\s -> s { wsWidth = Just (dp t 36), wsHeight = Just (dp t 36) }) (icon "ic_insert_drive_file")
                    , gap t 8
                    , middle $ styled (\s -> s { wsMaxWidth = Just (thScreenWidth t * 0.5) }) $ column $
                        maxLines 2 (label 16 (pText p) name)
                          : [ label 13 (pMuted p) (groupFileStatus mine file a) | Just a <- [attachment] ]
                          -- Nothing from a member downloads until asked for.
                          ++ [ pill t (pMine p) (hGroupFile h gid (groupFileId file)) "Download"
                             | not mine, Just a <- [attachment], attachmentState a `elem` [Waiting, Interrupted] ]
                          ++ [ pill t (pTheirs p) (hPauseGroupFile h gid (groupFileId file)) "Pause"
                             | not mine, Just a <- [attachment], attachmentState a == Moving ]
                    ]
                    ++ [gap t 8, stamp mine Delivered (entryTime this)] ]
          Said who kind body delivery _ ->
            let mine = isNothing who
                (by, said) = maybe ("You", shownText body) (`utter` shownText body) who
            in styled
              (\s -> s { wsBackgroundColor = Just (if mine then pMine p else pTheirs p)
                       , wsCornerRadius = Just (dp t 20), wsPadding = Just (dp t 12)
                       , wsMargin = Just (dp t bubbleMargin)
                       , wsAlignSelf = Just (if mine then SelfEnd else SelfStart) }) $ column $
              [ labelIn bold 13 (pAccent p) by | first, not mine ]
              ++
              [ row $
                  styled (\s -> s { wsMaxWidth = Just (thScreenWidth t * 0.6) })
                    (label 16 (pText p) (if kind == Tox.MessageAction then "* " <> by <> " " <> said else said))
                  : concat [ [gap t 8, stamp mine delivery (entryTime this)] | lastOfRun ]
              ]
          PrivateFrom who _ said -> privately False ("Privately from " <> memberLabel who) (shownText said) Delivered
          PrivateTo who _ said delivery -> privately True ("Privately to " <> memberLabel who) (shownText said) delivery
          _ -> styled (\s -> s { wsAlignSelf = Just SelfCenter, wsPadding = Just (dp t 4) }) $
                 label 13 (pMuted p) (entryPreview utter this)
      where
        privately mine heading said delivery = styled
          (\s -> s { wsBackgroundColor = Just (if mine then pMine p else pTheirs p)
                   , wsCornerRadius = Just (dp t 20), wsPadding = Just (dp t 12), wsMargin = Just (dp t bubbleMargin)
                   , wsAlignSelf = Just (if mine then SelfEnd else SelfStart) }) $ column
          [ labelIn bold 13 (pError p) heading
          , row [ styled (\s -> s { wsMaxWidth = Just (thScreenWidth t * 0.6) }) (label 16 (pText p) said)
                , gap t 8, stamp mine delivery (entryTime this) ]
          ]
    utter = uttered (clientProfile client) g
    pictureWidth = thScreenWidth t * 0.6
    pictureHeight path = case envPictureSize env path of
      Just (across, down) | across > 0 -> min (pictureWidth * 4 / 3) (pictureWidth * fromIntegral down / fromIntegral across)
      _ -> pictureWidth * 3 / 4
    -- Runs as in chats. Relayed messages run by the name relayed.
    together a b = case (entryWhat a, entryWhat b) of
      (Said x _ tx _ _, Said y _ ty _ _) -> fmap memberKey x == fmap memberKey y && fmap (author tx) x == fmap (author ty) y
                                        && day a == day b && diffUTCTime (entryTime b) (entryTime a) < 180
      _ -> False
    author body m = fst (utter m body)
    stamp mine delivery time = styled (\s -> s { wsAlignSelf = Just SelfEnd }) $ row $
      label 12 (pMuted p) (localTime env (clockFormat env) time)
        : concat [ [gap t 3, styled (\s -> s { wsWidth = Just (dp t 14), wsHeight = Just (dp t 14) })
                      (icon (if delivery == Pending then "ic_schedule" else "ic_done"))]
                 | mine ]

groupDetails :: Env -> Handlers -> Model -> GroupId -> Group -> Widget
groupDetails env h model gid@(GroupId chatKey) g = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) (capitalised (kindName (groupKind g))))]
  , grow $ scrollColumn $
      [ gap t 24
      , middle (avatar env 80 chatKey Nothing)
      , gap t 12
      , middle (label 24 (pText p) (groupLabel g))
      ]
      ++ [ middle (padded t 4 (label 14 (pMuted p) (groupTopic g))) | not (Text.null (groupTopic g)) ]
      ++ [ middle (padded t 4 (label 14 (pMuted p) (phaseText (groupPhase g)))) | groupPhase g /= InGroup ]
      ++ [ middle (padded t 4 (label 14 (pError p) notice)) | Just notice <- [modelNotice model] ]
      ++
      [ gap t 16
      , Tappable (hMuteGroup h gid) $ padded t 16 $ row
          [ middle (icon (if silent then "ic_notifications_off" else "ic_notifications"))
          , gap t 24
          , grow $ middle $ column $
              label 16 (pText p) (if silent then "Unmute notifications" else "Mute notifications")
                : [label 13 (pMuted p) until' | silent, Just until' <- [mutedUntil env (groupMute g)]]
          ]
      ]
      ++ [ Tappable (hToggleMentions h gid) $ padded t 16 $ row
             [ middle (icon (if groupMentions g then "ic_check_box" else "ic_check_box_outline_blank"))
             , gap t 24
             , grow (middle (label 16 (pText p) "Still notify me of mentions"))
             ]
         | silent ]
      -- Conferences can't share files.
      ++ [ Tappable (hGroupPictures h gid) $ padded t 16 $ row
             [ middle (icon "ic_image")
             , gap t 24
             , grow $ middle $ column
                 [ label 16 (pText p) "Send pictures"
                 , label 13 (pMuted p) $ maybe "As they are" (("As " <>) . formatName) (pictureFormat =<< groupPictures g)
                 ]
             ]
         | groupKind g == NgcGroup ]
      ++
      [ Tappable (hInviteFriends h gid) $ padded t 16 $ row
          [ middle (icon "ic_add")
          , gap t 24
          , grow (middle (label 16 (pText p) "Invite friends"))
          ]
      | groupPhase g == InGroup ]
      -- Conferences only have a title.
      ++
      [ Tappable (hEditTopic h gid) $ padded t 16 $ row
          [ middle (icon "ic_edit")
          , gap t 24
          , grow (middle (label 16 (pText p) "Change topic"))
          ]
      | groupKind g == NgcGroup, editable ]
      ++ [ padded t 16 (label 14 (pMuted p) audioNote) | groupKind g == AudioConference ]
      ++
      [ padded t 16 $ row
          [ grow $ middle $ column
              [ label 16 (pText p) (if groupKind g == NgcGroup then "Chat ID" else "Conference ID")
              , labelIn monospace 13 (pMuted p) chatKey
              ]
          , middle (iconButton t (hCopyChatId h gid) "ic_content_copy")
          ]
      ]
      ++ concat
      [ divider t
        : padded t 16 (label 14 (pAccent p) (memberCount (length members + 1)))
        : person (ownKey model) "You" (groupSelfRole g) Nothing
        : [ Tappable (hMember h gid n)
              (person (memberKey m) (memberLabel m) (Map.lookup n (groupRoles g)) (Map.lookup n (groupStatuses g)))
          | (n, m) <- members ]
      | groupPhase g == InGroup ]
      ++ [divider t]
      ++ relaying
      ++
      [ Tappable (hGroupSettings h gid) $ padded t 16 $ row
          [ middle (icon "ic_edit")
          , gap t 24
          , grow (middle (label 16 (pText p) "Group settings"))
          ]
      | groupPhase g == InGroup, groupSelfRole g == Just Tox.Founder, groupKind g == NgcGroup ]
      ++
      [ Tappable (hLeaveGroup h gid) $ padded t 16 $ row
          [ middle (icon "ic_exit_to_app")
          , gap t 24
          , grow (middle (label 16 (pError p) ("Leave " <> kindName (groupKind g))))
          ]
      | groupPhase g `elem` [Joining, InGroup] ]
      ++
      [ Tappable (hDeleteGroup h gid) $ padded t 16 $ row
          [ middle (icon "ic_delete")
          , gap t 24
          , grow (middle (label 16 (pError p) "Delete chat"))
          ]
      ]
  ]
  where
    t = envTheme env
    p = thPalette t
    silent = groupMuted (envNow env) g
    editable = groupPhase g == InGroup && groupMayTopic g
    senders = relayCounts g
    relaying = case groupRelayBot g of
      Just bot ->
        padded t 16 (label 14 (pAccent p) ("Relayed by " <> maybe "the bot" memberLabel (lookup bot [(memberKey m, m) | (m, _, _) <- senders])))
          : [ padded t 16 $ row
                [ grow (middle (label 16 (pText p) name))
                , middle (label 12 (pMuted p) (listTime env time))
                ]
            | (name, time) <- relayedThrough (clientProfile (modelClient model)) g ]
          ++ [ Tappable (hStopRelay h gid) $ padded t 16 $ row
                 [ middle (icon "ic_close")
                 , gap t 24
                 , grow (middle (label 16 (pText p) "Stop showing names from the relay bot"))
                 ] ]
      Nothing ->
        [ Tappable (hRelayBot h gid) $ padded t 16 $ row
            [ middle (icon "ic_group")
            , gap t 24
            , grow $ middle $ column
                [ label 16 (pText p) "Show names from a relay bot"
                , label 13 (pMuted p) "For a bot that carries messages from elsewhere as \"<name> text\""
                ]
            ]
        | any (\(_, relays, _) -> relays > 0) senders ]
    members = sortOn (\(n, m) -> (Map.findWithDefault Tox.User n (groupRoles g), Text.toCaseFold (memberLabel m)))
      (Map.toList (groupMembers g))
    person key name role status = padded t 12 $ row $
      [ middle (avatar env 40 key Nothing)
      , gap t 16
      , grow $ middle $ column $
          label 16 (pText p) name : [label 13 (pMuted p) s | Just s <- [statusName =<< status]]
      ]
      ++ [ middle (label 13 (pMuted p) r) | Just r <- [roleName =<< role] ]

groupSettingsScreen :: Env -> Handlers -> Model -> GroupId -> Group -> Widget
groupSettingsScreen env h model gid g = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Group settings")]
  , grow $ scrollColumn $ case groupSettings g of
      Nothing -> [padded t 16 (label 14 (pMuted p) "Tox doesn't have the group's settings yet")]
      Just current ->
        [ check (hPrivacy h gid) (Tox.groupPrivate current) "Private"
            (if Tox.groupPrivate current then "Friends join when invited" else "Anyone with its chat ID can join")
        , check (hTopicLock h gid) (Tox.groupTopicLock current) "Topic locked" "Only moderators may change the topic"
        , Tappable (hVoice h gid) $ padded t 16 $ column
            [label 16 (pText p) "Who may speak", label 13 (pMuted p) (voiceName (Tox.groupVoice current))]
        , padded t 16 $ column $
            [ label 12 (pAccent p) "Most members"
            , input InputNumber (hPeerLimit h) "" (modelPeerLimit model)
            , label 12 (pAccent p) "Password"
            , input InputPassword (hGroupPassword h) "" (modelGroupPassword model)
            , label 12 (pMuted p) "None if empty"
            ]
            ++ [label 12 (pError p) notice | Just notice <- [modelNotice model]]
        , styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
            row [pill t (pMine p) (hApplyGroupSettings h) "Save"]
        ]
  ]
  where
    t = envTheme env
    p = thPalette t
    check action on title about = Tappable action $ padded t 16 $ row
      [ middle (icon (if on then "ic_check_box" else "ic_check_box_outline_blank"))
      , gap t 24
      , grow $ middle $ column [label 16 (pText p) title, label 13 (pMuted p) about]
      ]

voiceName :: Tox.Voice -> Text
voiceName = \case
  Tox.VoiceAll        -> "Everyone but observers"
  Tox.VoiceModerators -> "Moderators"
  Tox.VoiceFounder    -> "The founder"

founding :: Env -> Handlers -> Model -> Widget
founding env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "New group")]
  , grow $ scrollColumn $
      [ padded t 16 $ column $
          [ label 12 (pAccent p) "Name"
          , input InputText (hGroupName h) "" (modelGroupName model)
          ]
          ++ [ label 12 (pError p) err
             | Just err <- [ if Text.null (modelGroupName model) then modelNotice model
                             else foundingError (modelFounding model) (modelGroupName model) ] ]
      ]
      ++ map choice [minBound .. maxBound]
      ++
      [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
          row [pill t (pMine p) (hFound h) "Create"]
      ]
  ]
  where
    t = envTheme env
    p = thPalette t
    choice kind = Tappable (hFounding h kind) $ padded t 16 $ row
      [ middle (icon (if modelFounding model == kind then "ic_radio_button_checked" else "ic_radio_button_unchecked"))
      , gap t 24
      , grow $ middle $ column [label 16 (pText p) title, label 13 (pMuted p) about]
      ]
      where
        (title, about) = case kind of
          PrivateGroup  -> ("Private group", "Friends join when you invite them")
          PublicGroup   -> ("Public group", "Anyone with its chat ID can join")
          NewConference -> ("Conference", "For friends whose apps have no groups")

joinGroup :: Env -> Handlers -> Model -> Widget
joinGroup env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Join a public group")]
  , padded t 16 $ column $
      [ label 12 (pAccent p) "Chat ID"
      , input InputText (hChatId h) "" (modelChatId model)
      ]
      ++ [label 12 (pError p) err | Just err <- [chatIdError (modelChatId model)]]
      ++
      [ label 12 (pAccent p) "Password"
      , input InputPassword (hPassword h) "" (modelPassword model)
      , label 12 (pMuted p) "Only if the group has one."
      ]
  , styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
      row [pill t (pMine p) (hJoinPublic h) "Join"]
  ]
  where
    t = envTheme env
    p = thPalette t

-- | Only friends who are online (Tox can only invite those).
inviteFriends :: Env -> Handlers -> Model -> GroupId -> Group -> Widget
inviteFriends env h model gid g = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) ("Invite to " <> groupLabel g))]
  , grow $ scrollColumn $
      [ padded t 16 (label 14 (pText p) notice) | Just notice <- [modelNotice model] ]
      ++ (if null online then [padded t 16 (label 14 (pMuted p) "No friends are online")] else map friend online)
  ]
  where
    t = envTheme env
    p = thPalette t
    online = [ (cid, c) | (cid, c) <- contactOrder (modelClient model), contactOnline c ]
    friend (cid@(ContactId key), c) = Tappable (hInviteFriend h gid cid) $ padded t 14 $ row
      [ middle (avatar env 48 key (Just True))
      , gap t 16
      , grow (middle (label 16 (pText p) (contactLabel cid c)))
      ]

-- | Tox may refuse the topic (the founder can lock it to moderators).
topicScreen :: Env -> Handlers -> Model -> Group -> Widget
topicScreen env h model g = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Topic")]
  , padded t 16 $ column
      [ label 12 (pAccent p) (groupName g)
      , input InputMultiline (hTopic h) "" (modelTopic model)
      ]
  , styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
      row [pill t (pMine p) (hApplyTopic h) "Save"]
  ]
  where
    t = envTheme env
    p = thPalette t

-- | Ask for the password up front. Tox only checks it after taking the
-- invite, and an invite only works once.
inviteScreen :: Env -> Handlers -> Model -> ContactId -> GroupId -> Invite -> Widget
inviteScreen env h model cid gid@(GroupId chatKey) invite = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Invite")]
  , grow $ scrollColumn $
      [ gap t 24
      , middle (avatar env 80 chatKey Nothing)
      , gap t 12
      , middle (label 24 (pText p) (inviteLabel invite))
      , middle (padded t 4 (label 14 (pMuted p) (friend <> " invited you to " <> article (kindName (inviteKind invite)))))
      ]
      -- Conferences have no password.
      ++ [ padded t 16 $ column
             [ label 12 (pAccent p) "Password"
             , input InputPassword (hPassword h) "" (modelPassword model)
             , label 12 (pMuted p) "Only if the group has one. A wrong one needs a new invite."
             ]
         | inviteKind invite == NgcGroup ]
      ++ [ padded t 16 (label 14 (pMuted p) audioNote) | inviteKind invite == AudioConference ]
      ++
      [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $ row
          [ pill t (pTheirs p) (hDecline h cid gid) "Decline"
          , gap t 8
          , pill t (pMine p) (hJoin h cid gid) "Join"
          ]
      ]
  ]
  where
    t = envTheme env
    p = thPalette t
    friend = maybe "A friend" (contactLabel cid) (Map.lookup cid (clientContacts (modelClient model)))

memberCount :: Int -> Text
memberCount n = Text.pack (show n) <> (if n == 1 then " member" else " members")

audioNote :: Text
audioNote = "Its audio isn't played: you take part by text."

article :: Text -> Text
article noun = (if Text.take 1 noun `elem` ["a", "e", "i", "o", "u"] then "an " else "a ") <> noun

-- | End of a mute, with the date unless it ends today.
mutedUntil :: Env -> Maybe Mute -> Maybe Text
mutedUntil env = \case
  Just (MutedUntil end) -> Just ("Muted until " <> listTime env end <> clock end)
  _                     -> Nothing
  where
    clock end
      | listTime env end == localTime env (clockFormat env) end = ""
      | otherwise = ", " <> localTime env (clockFormat env) end

-- | Over all screens while the call is hidden, also in other profiles. A tap
-- brings it back.
callBanner :: Env -> Action -> Call -> Widget
callBanner env returning c = Tappable returning $
  styled (\s -> s { wsBackgroundColor = Just (pOnline p), wsPadding = Just (dp t 12) }) $ row $
    case callPhase c of
      -- Only another profile's call can ring behind a banner.
      Ringing       -> [grow (label 16 white "Incoming call")]
      Talking since -> [grow returnTo, label 16 white (duration (round (diffUTCTime (envNow env) since)))]
      _             -> [grow returnTo, label 16 white "Calling\x2026"]
  where
    returnTo = label 16 white "Return to the call"
    t = envTheme env
    p = thPalette t
    white = Color 0xff 0xff 0xff 0xff

-- | Over every screen. The friend's video if they send it, else their name,
-- and ours in a corner.
callScreen :: Env -> Handlers -> Call -> PhoneCall -> Contact -> Maybe Text -> Widget
callScreen env h c phone contact notice = styled (\s -> s { wsBackgroundColor = Just (pBar p) }) $ column
  [ grow $ stack $
      ( if peerVideo
          then styled (\s -> s { wsAlignSelf = Just SelfStretch }) (video peerStream ScaleFill)
          else column $
            [ grow (text "")
            , middle (avatar env 120 key Nothing)
            , gap t 24
            , middle (label 28 (pText p) (contactLabel (callContact c) contact))
            , gap t 8
            , middle (label 16 (pMuted p) status)
            ]
            ++ [ middle (styled (\s -> s { wsPadding = Just (dp t 16) }) (label 14 (pError p) n)) | Just n <- [notice] ]
            ++ [ grow (text "") ] )
      -- Name and timer over their video.
      : [ styled (\s -> s { wsAlignSelf = Just SelfStretch }) $ column
            [ middle $ styled
                (\s -> s { wsBackgroundColor = Just (pScrim p), wsCornerRadius = Just (dp t 20)
                         , wsPadding = Just (dp t 12), wsMargin = Just (dp t 16) }) $ column
                [ middle (label 20 white (contactLabel (callContact c) contact))
                , middle (label 14 white status)
                ]
            , grow (text "")
            ]
        | peerVideo ]
      ++ [ styled (\s -> s { wsWidth = Just (dp t 108), wsHeight = Just (dp t 144), wsMargin = Just (dp t 16)
                          , wsAlignSelf = Just SelfEnd, wsCornerRadius = Just (dp t 12) })
             (video selfStream ScaleFill)
        | talking && callCamera c && not relayed ]
      ++ [ styled (\s -> s { wsAlignSelf = Just SelfStart, wsMargin = Just (dp t 8) })
             (iconButton t (hHideCall h) "ic_keyboard_arrow_down")
        | callPhase c /= Ringing ]
  , padded t 32 $ row [grow (column [middle b]) | b <- buttons]
  ]
  where
    talking = case callPhase c of
      Talking _ -> True
      _         -> False
    -- A linked device took it: we only pass it on.
    relayed = isJust (callDevice c)
    peerVideo = talking && callPeerVideo c && not relayed
    white = Color 0xff 0xff 0xff 0xff
    video stream scale = Image ImageConfig { icSource = ImageStream stream, icScaleType = scale }
    t = envTheme env
    p = thPalette t
    ContactId key = callContact c
    status = case callPhase c of
      _ | relayed, Talking since <- callPhase c -> "On a linked device \x00B7 " <> duration (round (diffUTCTime (envNow env) since))
        | relayed -> "Calling from a linked device\x2026"
      Calling -> "Calling\x2026"
      Ringing | callPeerVideo c -> "Incoming video call"
              | otherwise -> "Incoming voice call"
      Talking since -> duration (round (diffUTCTime (envNow env) since))
    buttons = case callPhase c of
      _ | relayed -> [roundButton t (pError p) (hHangUp h) "ic_call_end"]
      Ringing ->
        [ roundButton t (pError p) (hHangUp h) "ic_call_end"
        , roundButton t (pOnline p) (hAnswer h False) "ic_call"
        , roundButton t (pOnline p) (hAnswer h True) "ic_videocam"
        ]
      _ ->
        -- Nothing to switch without an earpiece (e.g. tablets).
        [ routeButton | length (callRoutes phone) > 1 ]
        ++ [ roundButton t (toggled (callCamera c)) (hCamera h)
               (if callCamera c then "ic_videocam" else "ic_videocam_off") ]
        ++ [ roundButton t (pTheirs p) (hFlipCamera h) "ic_flip_camera" | callCamera c ]
        ++
        [ roundButton t (toggled (callMuted c)) (hMute h)
            (if callMuted c then "ic_mic_off" else "ic_mic")
        , roundButton t (pError p) (hHangUp h) "ic_call_end"
        ]
    toggled on = if on then pMine p else pTheirs p
    routeButton
      | length (callRoutes phone) <= 2 = roundButton t (toggled (callRoute phone == Speaker)) (hRoute h) "ic_volume_up"
      | otherwise = roundButton t (pTheirs p) (hRoute h) $ case callRoute phone of
          Earpiece  -> "ic_call"
          Speaker   -> "ic_volume_up"
          Wired     -> "ic_headset"
          Bluetooth -> "ic_bluetooth"

-- | Keys of a chat's rows, so they stay on screen as earlier ones come in:
-- a message's from its ID, a day's heading from its day (below zero, so
-- they never meet).
messageKey :: MessageId -> Int
messageKey (MessageId bytes) = BS.foldl' (\n b -> n * 256 + fromIntegral b) 0 (BS.take 7 bytes)

dayKey :: Day -> Int
dayKey = negate . (+ 1) . fromIntegral . toModifiedJulianDay

-- | Pictures we show in the chat, by extension.
isPicture :: FilePath -> Bool
isPicture path = map toLower (takeExtension path) `elem` [".jpg", ".jpeg", ".png", ".gif", ".webp", ".bmp"]

fileStatus :: Bool -> Attachment -> Text
fileStatus mine file = byteSize (attachmentSize file) <> " \x00B7 " <> case attachmentState file of
  Waiting | mine      -> "Waiting"
          | otherwise -> "Offered"
  Moving -> Text.pack (show (percent (attachmentDone file) (attachmentSize file))) <> "%"
  Complete -> if mine then "Sent" else "Received"
  Interrupted -> "Paused until they're back online"
  Cancelled -> if mine then "Not sent" else "Not received"
  where
    percent done total = if total == 0 then 100 else done * 100 `div` total

-- | Ours is 'Moving' while we hash it, and a paused download waits for us,
-- not for a friend to come back.
groupFileStatus :: Bool -> GroupFile -> Attachment -> Text
groupFileStatus mine file a = case attachmentState a of
  Moving | mine -> byteSize (attachmentSize a) <> " \x00B7 Preparing"
         | groupFileRate file > 0 -> fileStatus mine a <> " \x00B7 " <> byteSize (floor (groupFileRate file)) <> "/s"
  Interrupted -> byteSize (attachmentSize a) <> " \x00B7 Paused"
  _ -> fileStatus mine a

-- | In the largest unit that keeps it at least 1.
byteSize :: Word64 -> Text
byteSize bytes = case dropWhile ((> fromIntegral bytes) . fst) units of
  (scale, unit) : _ | scale > 1 -> Text.pack (showFFloat (Just 1) (fromIntegral bytes / scale) "") <> " " <> unit
  _ -> Text.pack (show bytes) <> " B"
  where
    units = [(1024 ^ (3 :: Int), "GB"), (1024 ^ (2 :: Int), "MB"), (1024, "KB"), (1 :: Double, "B")]

profile :: Env -> Handlers -> Model -> Widget
profile env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Profile")]
  , grow $ scrollColumn . pure $ padded t 16 $ column $
      [ middle $ Tappable (hPickAvatar h) (avatar env 96 (ownKey model) Nothing)
      , gap t 8
      , middle $ row $
          pill t (pTheirs p) (hPickAvatar h) "Choose picture"
            : [ styled (\s -> s { wsMargin = Just (dp t 4) }) (pill t (pTheirs p) (hRemoveAvatar h) "Remove")
              | isJust (clientAvatar client) ]
      , gap t 16
      , centred (label 16 (pText p) "Nickname")
      , input InputText (hNickname h) "Nickname" (modelNickname model)
      , centred (label 16 (pText p) "Status message")
      , input InputText (hStatus h) "Status message" (modelStatus model)
      , Tappable (hAway h) $ padded t 12 $ row
          [ middle (icon (if profileAway current == Tox.Online then "ic_online_prediction" else "ic_schedule"))
          , gap t 24
          , grow $ middle $ column
              [label 16 (pText p) "Status", label 13 (pMuted p) (awayName (profileAway current))]
          ]
      ]
      ++ [label 12 (pError p) err | Just err <- [profileError (modelNickname model) (modelStatus model)]]
      ++
      [ styled (\s -> s { wsAlignSelf = Just SelfCenter, wsMargin = Just (dp t 12) }) $
          pill t (pMine p) (hApplyProfile h) $
            if applied then "Applied" else "Apply changes"
      , row
          [ grow $ middle $ column
              [ label 16 (pText p) "Tox ID"
              , labelIn monospace 14 (pMuted p) (profileToxId current)
              ]
          , middle (iconButton t (hCopyToxId h) "ic_content_copy")
          ]
      , gap t 16
      , middle $ styled (\s -> s { wsWidth = Just (dp t 200), wsHeight = Just (dp t 200) }) $
          Image ImageConfig { icSource = ImageData (envQr env), icScaleType = ScaleFit }
      , gap t 16
      , gap t 16
      , middle (pill t (pTheirs p) (hExport h) "Export profile (.tox)")
      , middle (label 12 (pMuted p) "The file holds your secret key: keep it safe.")
      ]
      ++ concat
      [ [gap t 16, middle (pill t (pTheirs p) (hOpenDevices h) "Linked devices")]
      | isNothing (clientLinked client) ]
      ++
      [ gap t 16
      , middle (label 12 (pMuted p) ("hTox " <> version))
      ]
  ]
  where
    client = modelClient model
    t = envTheme env
    p = thPalette t
    current = clientProfile client
    applied = Text.strip (modelNickname model) == profileNickname current
      && Text.strip (modelStatus model) == profileStatus current

-- | Which profiles go in the backup.
backup :: Env -> Handlers -> Model -> Widget
backup env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Back up profiles")]
  , grow $ scrollColumn $
      [ padded t 16 (label 14 (pError p) notice) | Just notice <- [modelNotice model] ]
      ++ map account (modelProfiles model)
      ++ [ padded t 16 $ label 13 (pMuted p)
             "Chats, contacts and keys, without the files. The backup isn't encrypted: keep it safe."
         | not (null (modelProfiles model)) ]
      ++ [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
             row [pill t (pMine p) (hBackUp h) "Back up"]
         | not (Set.null (modelBackup model)) ]
  ]
  where
    t = envTheme env
    p = thPalette t
    account a = Tappable (hToggleBackup h (accountId a)) $ padded t 16 $ row
      [ middle (icon (if Set.member (accountId a) (modelBackup model) then "ic_check_box" else "ic_check_box_outline_blank"))
      , gap t 24
      , grow $ middle $ column
          [label 16 (pText p) (accountName a), labelIn monospace 13 (pMuted p) (Text.take 8 (accountKey a))]
      ]

-- | Where time and memory go: the runtime, the busiest work, threads, what
-- hTox and Tox hold, and how calls go.
debugScreen :: Env -> Handlers -> Model -> Widget
debugScreen env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Debug")]
  , grow $ scrollColumn $ export : line ("Database version " <> Text.pack (show (envSchema env)))
      : [line ("Graphics: " <> envGraphics env) | not (Text.null (envGraphics env))] ++ case modelDebug model of
      Nothing -> [line "Measuring\x2026"]
      Just r -> concat
        [ heading "Memory" : maybe [line "The runtime keeps no statistics without -T."] memory (Debug.reportRuntime r)
        , heading ("Time in the last " <> Text.pack (show Lanes.windowSeconds) <> " s") : map lane (busiest r)
        , heading "Threads" : [ line (name <> " (" <> status <> ")" <> times n) | ((name, status), n) <- Map.toList (Debug.reportThreads r) ]
        , heading "hTox holds" : held
        , heading "Tox holds" : map size (filter ((> 0) . Tox.sizeCount) (Debug.reportSizes r))
        , heading "Traffic since Tox started" : map traffic (sortOn (negate . Tox.countBytes . snd) (Debug.reportTraffic r))
        , [ heading "Calls" | not (null (Debug.reportCalls r)) ] ++ map (line . Debug.callText) (Debug.reportCalls r)
        ]
  ]
  where
    t = envTheme env
    p = thPalette t
    heading = padded t 16 . label 16 (pAccent p)
    line = padded t 8 . label 14 (pText p)
    -- The last 30 minutes, every 10 seconds, as a file.
    export = styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
      row [pill t (pMine p) (hExportDebug h) "Export"]
    memory rt =
      [ line ("Heap " <> mb (Debug.rtLiveBytes rt) <> " after the last GC, at most " <> mb (Debug.rtMaxLive rt) <> " after a major one")
      , line (Text.pack (show (Debug.rtGcs rt)) <> " GCs (" <> Text.pack (show (Debug.rtMajorGcs rt)) <> " major), "
              <> percent (Debug.rtGcCpu rt) (Debug.rtGcCpu rt + Debug.rtMutatorCpu rt) <> " of CPU, "
              <> seconds (Debug.rtGcElapsed rt) <> " of " <> seconds (Debug.rtElapsed rt) <> " running")
      ]
    busiest r = sortOn (negate . Tox.laneTotal . snd) $ Map.toList (Debug.reportApp r) ++ Map.toList (Debug.reportTox r)
    lane (name, l) = line $ name <> ": " <> millis (Tox.laneTotal l) <> times (Tox.laneCount l) <> ", worst " <> millis (Tox.laneWorst l)
    times n = if n == 1 then "" else " \x00d7" <> Text.pack (show n)
    held = [ line (name <> ": " <> Text.pack (show n)) | (name, n) <- clientHolds (modelClient model) ]
    traffic (what, c) = line $
      Tox.countedName what <> ": " <> Text.pack (show (Tox.countTimes c)) <> ", " <> kb (Tox.countBytes c)
    kb b = Text.pack (show (b `div` 1024)) <> " KB"
    size s = padded t 8 $ label 14 (if Tox.overLimit s then pError p else pText p) $
      Tox.sizeName s <> ": " <> Text.pack (show (Tox.sizeCount s)) <> maybe "" ((" of " <>) . Text.pack . show) (Tox.sizeLimit s)
    mb b = Text.pack (show (b `div` 1048576)) <> " MB"
    millis n = Text.pack (showFFloat (Just 1) (fromIntegral n / 1e6 :: Double) "") <> " ms"
    seconds n = Text.pack (showFFloat (Just 1) (fromIntegral n / 1e9 :: Double) "") <> " s"
    percent a b = Text.pack (show (if b == 0 then 0 else a * 100 `div` b)) <> "%"

-- | Switch accounts, add one, or import one from another client.
profiles :: Env -> Handlers -> Model -> Widget
profiles env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Profiles")]
  , grow $ scrollColumn $
      [ padded t 16 (label 14 (pError p) notice) | Just notice <- [modelNotice model] ]
      ++ map account (modelProfiles model)
      ++ [ divider t
         , choice (hNewProfile h) "ic_add" "New profile"
         , choice (hImportProfile h) "ic_insert_drive_file" "Take a profile from a .tox file"
         , choice (hOpenBackup h) "ic_exit_to_app" "Back up profiles"
         , choice (hRestore h) "ic_insert_drive_file" "Restore profiles from a backup"
         , choice (hOpenLink h) "ic_link" "Link to an account on another device"
         ]
  ]
  where
    client = modelClient model
    t = envTheme env
    p = thPalette t
    account a
      | accountId a == clientProfileId client = padded t 16 $ row
          [ middle (avatar env 48 (ownKey model) Nothing)
          , gap t 16
          , grow $ middle $ column
              [ label 16 (pAccent p) (profileNickname (clientProfile client))
              , labelIn monospace 13 (pMuted p) (Text.take 8 (ownKey model))
              ]
          ]
      | otherwise = Tappable (hSwitchProfile h (accountId a)) $ padded t 16 $ row
          [ middle (avatar env 48 (accountKey a) Nothing)
          , gap t 16
          , grow $ middle $ column
              [ label 16 (pText p) (accountName a)
              , labelIn monospace 13 (pMuted p) (Text.take 8 (accountKey a))
              ]
          , middle (iconButton t (hDeleteProfile h (accountId a)) "ic_delete")
          ]
    choice action name caption = Tappable action $ padded t 16 $ row
      [middle (icon name), gap t 24, grow (middle (label 16 (pText p) caption))]

-- | Link to an account on another device (its home) with the pairing link
-- it shows.
linking :: Env -> Handlers -> Model -> Widget
linking env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Link an account")]
  , padded t 16 $ column $
      [ label 14 (pText p) "The other device runs the account on Tox, and this one shows its chats. On it, ask for a pairing link, then paste it here."
      , gap t 16
      , label 12 (pAccent p) "Pairing link"
      , input InputText (hLinkEdited h) "tox:…?pair=…" (modelLink model)
      ]
      ++ [label 12 (pError p) err | Just err <- [modelNotice model]]
  , styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
      row [pill t (pMine p) (hLink h) "Link"]
  ]
  where
    t = envTheme env
    p = thPalette t

-- | Other devices of the account sync through this one: it runs the account
-- on Tox, and they show its chats.
devicesScreen :: Env -> Handlers -> Model -> Widget
devicesScreen env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Linked devices")]
  , grow $ scrollColumn $
      [ padded t 16 $ label 14 (pText p)
          "Other devices can show this profile's chats, send for it and take its calls. This device stays \
          \the one on Tox: they sync and call through it while it's online."
      ]
      ++ (case pairing of
            Just link ->
              [ padded t 16 $ column
                  [ label 14 (pText p) "On the other device, link to an account on another device with this link, within 10 minutes. It works once."
                  , gap t 8
                  , row [ grow (middle (labelIn monospace 13 (pMuted p) link))
                        , middle (iconButton t (hCopyPairing h) "ic_content_copy") ]
                  , gap t 16
                  , middle $ styled (\s -> s { wsWidth = Just (dp t 200), wsHeight = Just (dp t 200) }) $
                      Image ImageConfig { icSource = ImageData (envQr env), icScaleType = ScaleFit }
                  ] ]
            Nothing ->
              [ styled (\s -> s { wsAlignSelf = Just SelfCenter, wsMargin = Just (dp t 12) }) $
                  pill t (pMine p) (hPairDevice h) "Link a device" ])
      ++ map device devices
  ]
  where
    t = envTheme env
    p = thPalette t
    home = clientHome (modelClient model)
    pairing = pairingLink model
    devices = maybe [] (Map.toList . Home.devices) home
    device (Sync.PublicKey pk, name) = padded t 16 $ row
      [ middle (icon "ic_link")
      , gap t 24
      , grow $ middle $ column
          [ label 16 (pText p) (if Text.null name then "Device" else name)
          , labelIn monospace 13 (pMuted p) (Text.take 8 (hexKey pk)) ]
      , middle (iconButton t (hRemoveDevice h (ContactId (hexKey pk))) "ic_delete")
      ]
    hexKey = Text.toUpper . Text.decodeUtf8 . Base16.encode

-- | Another client saved the profile with a password.
unlocking :: Env -> Handlers -> Model -> Widget
unlocking env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Import a profile")]
  , padded t 16 $ column $
      [ label 14 (pText p) "This profile was saved with a password. hTox keeps it in its own encrypted storage, without the password."
      , gap t 16
      , label 12 (pAccent p) "Password"
      , input InputPassword (hPassword h) "" (modelPassword model)
      ]
      ++ [label 12 (pError p) err | Just err <- [modelNotice model]]
  , styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
      row [pill t (pMine p) (hUnlock h) "Import"]
  ]
  where
    t = envTheme env
    p = thPalette t

-- | Proxy for TCP relays.
proxy :: Env -> Handlers -> Model -> Widget
proxy env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Proxy")]
  , grow $ scrollColumn $
      [ kind Nothing "None", kind (Just Tox.Socks5Proxy) "SOCKS5", kind (Just Tox.HttpProxy) "HTTP" ]
      ++ concat
        [ [ padded t 16 $ input InputText (hProxyHost h) "Host" (modelProxyHost model)
          , padded t 16 $ input InputNumber (hProxyPort h) "Port" (modelProxyPort model)
          , padded t 16 $ label 13 (pMuted p) "With a proxy, hTox makes no direct connections and doesn't look for friends on the local network."
          ]
        | isJust (modelProxyKind model) ]
      ++ [ padded t 16 (label 14 (pError p) notice) | Just notice <- [modelNotice model] ]
      ++ [ padded t 16 $ middle (pill t (pMine p) (hApplyProxy h) "Apply") ]
  ]
  where
    t = envTheme env
    p = thPalette t
    kind k name = Tappable (hProxyKind h k) $ padded t 16 $
      label 16 (if modelProxyKind model == k then pAccent p else pText p) name

proxyName :: Tox.Proxy -> Text
proxyName pr = named (Tox.proxyType pr) <> Text.pack (Tox.proxyHost pr) <> ":" <> Text.pack (show (Tox.proxyPort pr))
  where
    named = \case
      Tox.HttpProxy    -> "HTTP "
      Tox.Socks5Proxy  -> "SOCKS5 "
      Tox.GatewayProxy -> "Gateway "

-- | Account and device settings.
settings :: Env -> Handlers -> Model -> Widget
settings env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Settings")]
  , grow $ scrollColumn $
      [ heading "Files"
      , choice (hAutoAccept h) "Accept files without asking" $
          case settingAutoAccept (clientAccount client) of
            0    -> "Never"
            size -> "Up to " <> byteSize size
      , heading "Messages"
      , choice (hToggleTyping h) "Typing indicators"
          (onOff (settingTyping (clientAccount client)) <> ". You see when friends are writing, and they see when you are")
      , heading "On this device"
      , choice (hToggleStay h) "Stay online"
          (if settingStay device then "While hTox is closed, and once the phone starts" else "Only while hTox is open or in a call")
      , choice (hToggleUdp h) "Connect directly (UDP)" (onOff (settingUdp device))
      , choice (hToggleIpv6 h) "Use IPv6" (onOff (settingIpv6 device))
      , choice (hToggleLan h) "Find friends on the local network" (onOff (settingLan device))
      , choice (hOpenProxy h) "Proxy" (maybe "None" proxyName (settingProxy device))
      , padded t 16 $ label 13 (pMuted p) "Network changes apply when hTox starts again."
      ]
  ]
  where
    client = modelClient model
    t = envTheme env
    p = thPalette t
    device = clientDevice client
    heading name = padded t 16 $ label 14 (pAccent p) name
    choice action name value = Tappable action $ padded t 16 $ column
      [label 16 (pText p) name, label 14 (pMuted p) value]
    onOff on = if on then "On" else "Off"

-- | Who to forward to, latest chat first.
forward :: Env -> Handlers -> Model -> Widget
forward env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Forward to")]
  , grow $ scrollColumn
      [ Tappable (hForwardTo h cid) $ padded t 14 $ row
          [ middle (avatar env 40 key (Just (contactOnline contact)))
          , gap t 16
          , grow (middle (label 16 (pText p) (contactLabel cid contact)))
          ]
      | (cid@(ContactId key), contact) <- contactOrder (modelClient model) ]
  ]
  where
    t = envTheme env
    p = thPalette t

-- | A message and what we know of it.
infoScreen :: Env -> Handlers -> Text -> [(Text, Text)] -> Widget
infoScreen env h said facts = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Message")]
  , grow $ scrollColumn $ padded t 16 (label 16 (pText p) said) : map fact facts
  ]
  where
    t = envTheme env
    p = thPalette t
    fact (name, value) = padded t 16 $ column [label 16 (pText p) name, label 14 (pMuted p) value]

-- | Date and time in full.
fullTime :: Env -> UTCTime -> Text
fullTime env time = localTime env "%b %-d, %Y " time <> localTime env (clockFormat env) time

-- | When it was sent and delivered, and who sent it.
messageInfo :: Env -> Handlers -> ContactId -> Contact -> Message -> Widget
messageInfo env h cid@(ContactId key) contact m = infoScreen env h (preview (contactLabel cid contact) m) facts
  where
    when' = fullTime env
    facts
      | messageMine m =
          (if messageDelivery m == Pending then "Sending" else "Sent", when' (messageTime m))
            : [("Delivered", when' d) | Just d <- [messageDelivered m]]
      | otherwise =
          [ ("Received", when' (messageTime m))
          , ("From", contactLabel cid contact <> "\n" <> key) ]
          -- What the chat doesn't show, as Tox carried it.
          ++ [ ("Hidden after a zero byte", Text.decodeUtf8 (Base16.encode (BS.drop 1 hidden)))
             | let hidden = BS.dropWhile (/= 0) (fromMaybe (Text.encodeUtf8 (messageText m)) (messageRaw m))
             , not (BS.null hidden) ]

-- | When it was sent or came, and who sent it (relayed ones by the name
-- relayed). Groups confirm no delivery.
groupMessageInfo :: Env -> Handlers -> Model -> Group -> GroupEntry -> Widget
groupMessageInfo env h model g e = infoScreen env h (fromMaybe "" (entryText e)) $ case entryWhat e of
  Said Nothing _ _ delivery _ -> [sending delivery]
  Said (Just m) _ body _ _    -> [received, ("From", fst (uttered (clientProfile (modelClient model)) g m (shownText body)) <> "\n" <> memberKey m)]
  PrivateFrom m _ _           -> [received, ("Privately from", memberLabel m <> "\n" <> memberKey m)]
  PrivateTo m _ _ delivery    -> [sending delivery, ("Privately to", memberLabel m <> "\n" <> memberKey m)]
  _                           -> []
  where
    sending delivery = (if delivery == Pending then "Sending" else "Sent", fullTime env (entryTime e))
    received = ("Received", fullTime env (entryTime e))

details :: Env -> Handlers -> ContactId -> Contact -> Widget
details env h cid@(ContactId key) contact = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Contact")]
  , grow $ scrollColumn $
      [ gap t 24
      , middle (avatar env 80 key (Just (contactOnline contact)))
      , gap t 12
      , middle (label 24 (pText p) (contactLabel cid contact))
      ]
      ++ [ middle (padded t 4 (label 14 (pMuted p) (contactStatusMessage contact)))
         | not (Text.null (contactStatusMessage contact)) ]
      ++ [ middle (label 14 (pMuted p) away)
         | contactOnline contact, Just away <- [statusName (contactStatus contact)] ]
      ++
      [ gap t 16
      , Tappable (hMuteChat h cid) $ padded t 16 $ row
          [ middle (icon (if silent then "ic_notifications_off" else "ic_notifications"))
          , gap t 24
          , grow $ middle $ column $
              label 16 (pText p) (if silent then "Unmute notifications" else "Mute notifications")
                : [label 13 (pMuted p) until' | silent, Just until' <- [mutedUntil env (contactMute contact)]]
          ]
      , Tappable (hPictures h cid) $ padded t 16 $ row
          [ middle (icon "ic_image")
          , gap t 24
          , grow $ middle $ column
              [ label 16 (pText p) "Send pictures"
              , label 13 (pMuted p) $ maybe "As they are" (("As " <>) . formatName)
                  (pictureFormat =<< contactPictures contact)
              ]
          ]
      , padded t 16 $ row
          [ grow $ middle $ column
              [ label 16 (pText p) "Public key"
              , labelIn monospace 13 (pMuted p) key
              ]
          , middle (iconButton t (hCopyKey h cid) "ic_content_copy")
          ]
      , divider t
      , Tappable (hDelete h cid) $ padded t 16 $ row
          [ middle (icon "ic_delete")
          , gap t 24
          , grow (middle (label 16 (pError p) "Delete contact"))
          ]
      ]
  ]
  where
    t = envTheme env
    p = thPalette t
    silent = muted (envNow env) contact

addContact :: Env -> Handlers -> Model -> Widget
addContact env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Add contact")]
  , padded t 16 $ column $
      [ label 12 (pAccent p) "Tox ID"
      , input InputText (hAddress h) "" (modelAddress model)
      ]
      ++ [label 12 (pError p) err | Just err <- [addressError (profileToxId (clientProfile (modelClient model))) (modelAddress model)]]
      ++
      [ label 12 (pMuted p) "Message"
      , input InputText (hRequest h) "" (modelRequest model)
      ]
      ++ [label 12 (pError p) err | Just err <- [requestError (modelRequest model)]]
      ++ [label 12 (pError p) err | Just err <- [modelAddError model]]
      ++
      [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsMargin = Just (dp t 12) }) $
          pill t (pMine p) (hAddFriend h) "Add"
      ]
  ]
  where
    t = envTheme env
    p = thPalette t

startFailure :: Theme -> Action -> Text -> Widget
startFailure t retry reason = styled (\s -> s { wsBackgroundColor = Just (pBackground p) }) $
  padded t 24 $ column
    [ label 22 (pText p) "hTox couldn't start"
    , gap t 12
    , label 14 (pText p) reason
    , gap t 12
    , label 14 (pMuted p) "Your chats are kept as they were."
    , styled (\s -> s { wsAlignSelf = Just SelfEnd, wsMargin = Just (dp t 12) }) $
        pill t (pMine p) retry "Try again"
    ]
  where p = thPalette t

lastMessage :: Contact -> Maybe Message
lastMessage contact = case reverse (contactMessages contact) of
  m : _ -> Just m
  []    -> Nothing

dayHeading :: Env -> UTCTime -> Text
dayHeading env time
  | days == 0 = "Today"
  | days == 1 = "Yesterday"
  | year time == year (envNow env) = localTime env "%a, %b %-d" time
  | otherwise = localTime env "%b %-d, %Y" time
  where
    days = diffDays (dayOf (envNow env)) (dayOf time)
    dayOf = localDay . utcToLocalTime (envZone env)
    year = localTime env "%Y"

clockFormat :: Env -> String
clockFormat env = if envClock24 env then "%H:%M" else "%-l:%M %p"

listTime :: Env -> UTCTime -> Text
listTime env time
  | days == 0 = localTime env (clockFormat env) time
  | days < 7 = localTime env "%a" time
  | otherwise = localTime env "%b %-d" time
  where
    days = diffDays (dayOf (envNow env)) (dayOf time)
    dayOf = localDay . utcToLocalTime (envZone env)

localTime :: Env -> String -> UTCTime -> Text
localTime env format =
  Text.pack . formatTime defaultTimeLocale format . utcToLocalTime (envZone env)

ownKey :: Model -> Text
ownKey = Text.take 64 . profileToxId . clientProfile . modelClient

-- | Identicon in a circle of @size@ dp. Friends get a dot while online.
avatar :: Env -> Double -> Text -> Maybe Bool -> Widget
avatar env size key online = stack $
  styled (\s -> s { wsWidth = Just (dp t size), wsHeight = Just (dp t size)
                  , wsCornerRadius = Just (dp t size / 2) })
    (Image ImageConfig { icSource = ImageData (envAvatar env key), icScaleType = ScaleFill })
  : [ styled (\s -> s { wsWidth = Just (dp t 14), wsHeight = Just (dp t 14)
                    , wsCornerRadius = Just (dp t 7), wsAlignSelf = Just SelfEnd
                    , wsBackgroundColor = Just (pOnline (thPalette t)) })
      (text "")
  | online == Just True ]
  where t = envTheme env

appBar :: Theme -> [Widget] -> Widget
appBar t = styled (\s -> s { wsHeight = Just (dp t 64), wsPadding = Just (dp t 4) })
  . row . map middle

roundButton :: Theme -> Color -> Action -> Text -> Widget
roundButton t colour action name = Tappable action $ styled
  (\s -> s { wsBackgroundColor = Just colour, wsPadding = Just (dp t 18), wsCornerRadius = Just (dp t 30)
           , wsWidth = Just (dp t 60), wsHeight = Just (dp t 60) })
  (icon name)

iconButton :: Theme -> Action -> Text -> Widget
iconButton t action name = Tappable action $ styled
  (\s -> s { wsPadding = Just (dp t 12), wsCornerRadius = Just (dp t 24) })
  (icon name)

icon :: Text -> Widget
icon name = Image ImageConfig
  { icSource = ImageResource (ResourceName name), icScaleType = ScaleFit }

pill :: Theme -> Color -> Action -> Text -> Widget
pill t colour action caption = styled
  (\s -> s { wsBackgroundColor = Just colour, wsCornerRadius = Just (dp t 24) })
  (button caption action)

divider :: Theme -> Widget
divider t = styled (\s -> s { wsHeight = Just 1, wsBackgroundColor = Just (pDivider (thPalette t))
                            , wsAlignSelf = Just SelfStretch })
  (text "")

gap :: Theme -> Double -> Widget
gap t size = styled (\s -> s { wsWidth = Just (dp t size), wsHeight = Just (dp t size) }) (text "")

label :: Double -> Color -> Text -> Widget
label = labelIn id

labelIn :: (FontConfig -> FontConfig) -> Double -> Color -> Text -> Widget
labelIn font size colour txt = Text TextConfig
  { tcLabel = txt
  , tcFontConfig = Just (font (fontOfSize size))
  , tcTextColor = Just colour
  }

bold, monospace :: FontConfig -> FontConfig
bold f = f { fontWeight = FontBold }
monospace f = f { fontFamily = FontMonospace }

input :: InputType -> OnChange -> Text -> Text -> Widget
input kind onChange hint value = TextInput TextInputConfig
  { tiInputType = kind
  , tiHint = hint
  , tiValue = value
  , tiOnChange = onChange
  , tiFontConfig = Just (fontOfSize 16)
  , tiTextColor = Nothing
  , tiAutoFocus = False
  }

styled :: (WidgetStyle -> WidgetStyle) -> Widget -> Widget
styled f = Styled (f defaultStyle)

-- | At most @n@ lines, then an ellipsis.
maxLines :: Int -> Widget -> Widget
maxLines n = styled (\s -> s { wsMaxLines = Just n })

padded :: Theme -> Double -> Widget -> Widget
padded t size = styled (\s -> s { wsPadding = Just (dp t size) })

-- | Takes the space left in its row or column.
grow :: Widget -> Widget
grow = styled (\s -> s { wsWeight = Just 1 })

middle :: Widget -> Widget
middle = styled (\s -> s { wsAlignSelf = Just SelfCenter })

centred :: Widget -> Widget
centred = styled (\s -> s { wsTextAlign = Just AlignCenter })
