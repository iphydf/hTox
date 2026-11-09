{-# LANGUAGE OverloadedStrings #-}
module HTox.ViewSpec (spec) where

import qualified Data.ByteString     as BS
import           Data.Maybe          (mapMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Time           (UTCTime (..), fromGregorian, utc)
import           Hatter.Action       (Action (..))
import           Hatter.Widget
import           Test.Hspec

import qualified Tox.Application.Tox as Tox
import qualified Tox.Client.Update   as Client
import qualified Tox.Share.Info      as Info

import           HTox.Model
import           HTox.Sample
import           HTox.Taps           (handlers, tapped)
import qualified HTox.Taps           as T
import           HTox.Theme          (theme)
import           HTox.View

env :: Env
env = Env
  { envTheme = theme 1 1080 False
  , envNow = UTCTime (fromGregorian 2026 11 2) 43200
  , envZone = utc
  , envAvatar = const BS.empty
  , envQr = BS.empty
  , envPictureSize = const Nothing
  , envDecoded = const Nothing
  , envClock24 = False
  , envElsewhere = Nothing
  , envOthers = []
  , envSchema = 3
  , envGraphics = ""
  }

shown :: Model -> Widget
shown = view env handlers

model' :: [Event] -> Model
model' events = applying events sample

children :: Widget -> [Widget]
children widget = case widget of
  Column s              -> map liWidget (lsWidgets s)
  Row s                 -> map liWidget (lsWidgets s)
  Stack items           -> map liWidget items
  LazyColumn items      -> map liWidget items
  Styled _ child        -> [child]
  Animated _ child      -> [child]
  Tappable _ child      -> [child]
  LongPressable _ child -> [child]
  Scrolled _ _ child    -> [child]
  _                     -> []

texts :: Widget -> [Text]
texts widget = case widget of
  Text c      -> [tcLabel c]
  Button c    -> [bcLabel c]
  TextInput c -> [tiValue c]
  _           -> concatMap texts (children widget)

-- | Each tap's action, with the texts it covers.
taps :: Widget -> [([Text], Action)]
taps widget = case widget of
  Tappable action child -> (texts child, action) : taps child
  Button c              -> [([bcLabel c], bcAction c)]
  _                     -> concatMap taps (children widget)

tapOn :: Text -> Widget -> Maybe T.Tapped
tapOn caption widget =
  -- The innermost one gets the tap.
  case reverse [action | (covered, action) <- taps widget, caption `elem` covered] of
    action : _ -> tapped action
    []         -> Nothing

-- | What each tap does, in order.
tappedIn :: Widget -> [T.Tapped]
tappedIn = mapMaybe (tapped . snd) . taps

-- | What each long press does, in order.
heldIn :: Widget -> [T.Tapped]
heldIn widget = case widget of
  LongPressable action child -> maybe id (:) (tapped action) (heldIn child)
  _                          -> concatMap heldIn (children widget)

spec :: Spec
spec = do
  it "lists invites and groups, and a group's messages under their senders' names" $ do
    let dana = Member (Text.replicate 8 "DA7A0000") "Dana"
        joined = [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
                 , ClientEvent (Client.MemberJoined team (at 3 9 0) 1 dana), ClientEvent (Client.GroupConnected team (at 3 9 0))
                 , ClientEvent (Client.GroupReceived team (at 3 9 1) 1 Tox.MessageNormal "hi" Nothing)
                 , ClientEvent (Client.GroupReceived team (at 3 9 2) 1 Tox.MessageNormal "again" Nothing) ]
        listed = shown (model' (joined ++ [Back, ClientEvent (Client.Invited bob (GroupId "C0DE") NgcGroup "Other" "other invite")]))
    tapOn "Other" listed `shouldBe` Just (T.OpenInvite bob (GroupId "C0DE"))
    tapOn "Team" listed `shouldBe` Just (T.OpenGroup team)
    heldIn listed `shouldContain` [T.HoldGroup team]
    texts listed `shouldContain` ["Bob invited you to a group"]
    let chatTexts = texts (shown (model' joined))
    filter (`elem` ["Dana", "hi", "again", "2 members"]) chatTexts `shouldBe` ["2 members", "Dana", "hi", "again"]
    let topical allowed = shown (model' (joined ++ map ClientEvent
          [Client.TopicChanged team (at 3 9 3) (Just 1) "Haskell", Client.TopicAllowed team allowed] ++ [Open (GroupDetails team)]))
    texts (topical False) `shouldContain` ["Haskell"]
    texts (topical False) `shouldNotContain` ["Change topic"]
    tapOn "Change topic" (topical True) `shouldBe` Just (T.EditTopic team)
    let away = joined ++ [ClientEvent (Client.MemberStatus team 1 Tox.Away), Open (GroupDetails team)]
    texts (shown (model' away)) `shouldContain` ["Dana", "Away"]
    texts (shown (model' (away ++ [ClientEvent (Client.MemberStatus team 1 Tox.Online)]))) `shouldNotContain` ["Away"]

  it "selects, answers and tells of what members wrote in a group, not of who went" $ do
    let dana = Member (Text.replicate 8 "DA7A0000") "Dana"
        said = [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
               , ClientEvent (Client.MemberJoined team (at 3 9 0) 1 dana), ClientEvent (Client.GroupConnected team (at 3 9 0))
               , ClientEvent (Client.GroupReceived team (at 3 9 1) 1 Tox.MessageNormal "hi" Nothing)
               , ClientEvent (Client.MemberLeft team (at 3 9 2) 1 Tox.ExitQuit) ]
        hi = entryAt team 0 (model' said)
    tapOn "hi" (shown (model' said)) `shouldBe` Just (T.TapEntry team hi)
    tapOn "Dana left" (shown (model' said)) `shouldBe` Nothing
    texts (shown (model' (said ++ [Select hi]))) `shouldContain` ["1 selected"]
    texts (shown (model' (said ++ [Reply hi]))) `shouldContain` ["> hi"]
    texts (shown (model' (said ++ [Open (MessageInfo (Client.GroupChat team) hi)])))
      `shouldContain` ["Received", "Nov 3, 2026 9:01 AM", "From", "Dana\n" <> memberKey dana]

  it "shows members coming and going in a row as one line, and one or two as they are" $ do
    let member n = Member (Text.replicate 8 (Text.pack (show n) <> "0000000")) (Text.pack ["ABCDE" !! fromIntegral n])
        joined = [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
                 , ClientEvent (Client.GroupConnected team (at 3 9 0)) ]
        coming n m = ClientEvent (Client.MemberJoined team (at 3 10 m) n (member n))
        going n m = ClientEvent (Client.MemberLeft team (at 3 10 m) n Tox.ExitQuit)
        busy = joined ++ [coming 0 0, coming 1 1, coming 2 2, coming 3 3, going 0 4, coming 0 5, coming 4 6]
    filter (Text.isInfixOf "joined") (texts (shown (model' busy))) `shouldBe` ["A, B and 3 others joined \x00B7 A left"]
    filter (Text.isInfixOf "joined") (texts (shown (model' (joined ++ [coming 0 0, coming 1 1])))) `shouldBe` ["A joined", "B joined"]

  it "shows a file shared in a group, downloading it only when tapped" $ do
    let dana = Member (Text.replicate 8 "DA7A0000") "Dana"
        inf = Info.Info "notes.txt" 2048 2048 (BS.replicate 20 1)
        fid = Info.hash inf
        shared = [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
                 , ClientEvent (Client.MemberJoined team (at 3 9 0) 1 dana), ClientEvent (Client.GroupConnected team (at 3 9 0))
                 , ClientEvent (Client.GroupFileShared team (at 3 9 1) 1 6 fid), ClientEvent (Client.GroupFileLearned team fid inf) ]
        chat = shown (model' shared)
    texts chat `shouldContain` ["notes.txt", "2.0 KB \x00B7 Offered"]
    tapOn "Download" chat `shouldBe` Just (T.GroupFileDownload team fid)

  it "attaches with the chat's one button until there is something to send" $ do
    let empty = tappedIn (shown (model' [Open (Chat alice)]))
        written = tappedIn (shown (model' [Open (Chat alice), DraftChanged "hi" (at 1 0 0)]))
    (T.Attach `elem` empty, T.Send `elem` empty) `shouldBe` (True, False)
    (T.Attach `elem` written, T.Send `elem` written) `shouldBe` (False, True)

  it "lists the contacts, each opening its chat and holding its menu" $ do
    texts (shown sample) `shouldContain` ["Alice", "Looks good already.", "9:02 AM", "1"]
    tapOn "Bob" (shown sample) `shouldBe` Just (T.OpenChat bob)
    heldIn (shown sample) `shouldContain` [T.HoldChat alice, T.HoldChat bob]

  it "lists the other profiles in the drawer, to switch to" $ do
    let work = ProfileSummary "Work" "AB12" Nothing ViaUdp 3
        page = view env { envOthers = [(ProfileId 2, work)] } handlers (model' [OpenDrawer])
    mapM_ (\t -> texts page `shouldContain` [t]) ["Work", "Online", "3"]
    tapOn "Work" page `shouldBe` Just (T.SwitchProfile (ProfileId 2))

  it "names the app and the profile over the chats" $
    texts (shown sample) `shouldContain` ["hTox (Tox User)"]

  it "shows a message only up to its first zero byte, and what's hidden in its details" $ do
    let received = [ClientEvent (Client.Received alice (at 3 9 0) Tox.MessageNormal "hi\0secret")]
        screens = [model' received, model' (received ++ [Open (Chat alice)])]
    map (elem "hi" . texts . shown) screens `shouldBe` [True, True]
    any (Text.isInfixOf "secret") (concatMap (texts . shown) screens) `shouldBe` False
    texts (shown (model' (received ++ [Open (MessageInfo (Client.FriendChat alice) (messageAt alice 3 (model' received)))])))
      `shouldContain` ["Hidden after a zero byte", "736563726574"]

  it "shows a friend typing in the chats and in their chat, until they stop" $ do
    let typing = [ClientEvent (Client.FriendTyping alice True)]
    texts (shown (model' typing)) `shouldContain` ["typing\x2026"]
    texts (shown (model' (typing ++ [Open (Chat alice)]))) `shouldContain` ["typing\x2026"]
    texts (shown (model' (typing ++ [ClientEvent (Client.FriendTyping alice False)]))) `shouldNotContain` ["typing\x2026"]

  it "names a contact by the start of its key until it has a name" $
    tapOn "19A974C2" (shown (model' [ClientEvent (Client.AcceptRequest carol)])) `shouldBe` Just (T.OpenChat carol)

  it "lists friend requests to accept or reject" $ do
    texts (shown sample) `shouldContain` ["Hey, I'm Carol, come chat with me on Tox!"]
    tappedIn (shown sample) `shouldContain` [T.Accept carol, T.Reject carol]

  it "shows a hidden call's chat under a banner that returns to it" $ do
    let page = view env { envNow = at 3 9 2 } handlers
          (model' [Open (Chat alice), StartCall (at 3 9 0) False, ClientEvent (Client.CallPeer alice (at 3 9 0) False), HideCall])
    mapM_ (\t -> texts page `shouldContain` [t]) ["Return to the call", "2:00", "Looks good already."]
    tapOn "Return to the call" page `shouldBe` Just T.ShowCall

  it "shows another profile's call under a banner that goes to it" $ do
    let call = clientCall (modelClient (model' [Open (Chat alice), StartCall (at 3 9 0) False, ClientEvent (Client.CallPeer alice (at 3 9 0) False)]))
        page = view env { envNow = at 3 9 2, envElsewhere = call } handlers sample
    mapM_ (\t -> texts page `shouldContain` [t]) ["Return to the call", "2:00", "Bob"]
    tapOn "Return to the call" page `shouldBe` Just T.ReturnToCall
    let ringing = view env { envElsewhere = clientCall (modelClient (model' [ClientEvent (Client.CallInvited alice (at 3 9 0) False)])) } handlers sample
    tapOn "Incoming call" ringing `shouldBe` Just T.ReturnToCall

  it "lists calls newest first, calling back those online" $ do
    let made = [ ClientEvent (Client.CallInvited bob (at 3 9 0) False), ClientEvent (Client.CallEnded bob (at 3 9 1))
                  , Open (Chat alice), StartCall (at 3 10 0) False, ClientEvent (Client.CallPeer alice (at 3 10 0) False), ClientEvent (Client.HangUp (at 3 10 2)) ]
        page = shown (model' (made ++ [Open CallsScreen]))
    [cid | (cid, _, _) <- callHistory (modelClient (model' made))] `shouldBe` [alice, bob]
    tapOn "Bob" page `shouldBe` Just (T.OpenChat bob)
    [t | t@(T.CallBack _) <- tappedIn page] `shouldBe` [T.CallBack alice]
    texts (shown (model' [Open CallsScreen])) `shouldContain` ["No calls yet"]
    tapOn "Allow" (shown (model' [Open CallsScreen, FullScreenChecked False])) `shouldBe` Just T.AllowFullScreen

  it "shows a few emoji alone large" $ do
    let sizes w = case w of
          Text c -> [(tcLabel c, fontSize <$> tcFontConfig c)]
          _      -> concatMap sizes (children w)
        page = shown (model' [Open (Chat alice), ClientEvent (Client.Received alice (at 2 9 3) Tox.MessageNormal "\xF0\x9F\x91\x8D")])
    sizes page `shouldContain` [("\x1F44D", Just 48)]

  it "lists the profiles, switching to the others" $ do
    let page = shown (model' [ProfilesChanged [Account (ProfileId 0) "Tox User" "", Account (ProfileId 7) "Work" (Text.replicate 8 "19A974C2")], Open ProfilesScreen])
    texts page `shouldContain` ["Tox User"]
    tapOn "Tox User" page `shouldBe` Nothing
    tapOn "Work" page `shouldBe` Just (T.SwitchProfile (ProfileId 7))
    tappedIn page `shouldContain` [T.DeleteProfile (ProfileId 7), T.NewProfile, T.ImportProfile]

  it "backs up the chosen profiles" $ do
    let accounts = ProfilesChanged [Account (ProfileId 0) "Tox User" "", Account (ProfileId 7) "Work" (Text.replicate 8 "19A974C2")]
        page = shown (model' [accounts, Open BackupScreen, ToggleBackup (ProfileId 0)])
    tapOn "Work" page `shouldBe` Just (T.ToggleBackup (ProfileId 7))
    tapOn "Back up" page `shouldBe` Just T.BackUp
    tapOn "Back up" (shown (model' [accounts, Open BackupScreen, ToggleBackup (ProfileId 0), ToggleBackup (ProfileId 7)]))
      `shouldBe` Nothing

  it "asks for a profile's password, showing when it was wrong" $ do
    let page = shown (model' [Open UnlockScreen, PasswordChanged "secret"])
    tapOn "Import" page `shouldBe` Just T.Unlock
    texts (shown (model' [Open UnlockScreen, Notify "Wrong password"])) `shouldContain` ["Wrong password"]
    modelScreen (model' [Open UnlockScreen, Back]) `shouldBe` ProfilesScreen

  it "pairs a device with a link, and lists the devices to remove" $ do
    let pairing = [Open ProfileScreen, Open DevicesScreen, ClientEvent (Client.Pair "c0de" (at 3 9 0))]
        tablet = ContactId (Text.replicate 32 "DD")
    tapOn "Linked devices" (shown (model' [Open ProfileScreen])) `shouldBe` Just T.OpenDevices
    tapOn "Link a device" (shown (model' [Open DevicesScreen])) `shouldBe` Just T.PairDevice
    texts (shown (model' pairing)) `shouldContain` ["tox:" <> toxId (Text.replicate 8 "5A17ED00") "0BADF00D" <> "?pair=c0de"]
    let paired = shown (model' (pairing ++ [ClientEvent (Client.Requested tablet "pair:c0de")]))
    texts paired `shouldContain` ["Device", "DDDDDDDD"]
    tapOn "Link a device" paired `shouldBe` Just T.PairDevice
    tappedIn paired `shouldContain` [T.RemoveDevice tablet]

  it "says there are no chats yet" $
    texts (shown initial) `shouldContain` ["No chats yet"]

  it "opens the drawer over the contacts, with the profile" $ do
    let drawer = shown (model' [OpenDrawer])
    texts drawer `shouldContain` ["Tox User", "Toxing on Haskell"]
    tapOn "Toxing on Haskell" drawer `shouldBe` Just T.OpenProfile

  it "shows the chat's messages by day, and its draft" $ do
    let model = model' [Open (Chat alice), DraftChanged "typing" (at 1 0 0)]
    mapM_ (\t -> texts (shown model) `shouldContain` [t])
      [ "Yesterday", "Hi! Is this the Haskell client?", "3:14 PM"
      , "Today", "Looks good already.", "typing" ]

  it "shows times on the clock the user wants, and older days with their date" $ do
    let model = model' [Open (Chat alice)]
    texts (view env { envClock24 = True } handlers model) `shouldContain` ["15:14"]
    texts (view env { envNow = at 9 12 0 } handlers model) `shouldContain` ["Sun, Nov 1"]
    texts (view env { envNow = UTCTime (fromGregorian 2027 1 5) 0 } handlers model)
      `shouldContain` ["Nov 1, 2026"]

  it "shows the time of a run's last message only" $ do
    let run = [Open (Chat bob), DraftChanged "one" (at 1 0 0), Send (at 2 12 0), DraftChanged "two" (at 1 0 0), Send (at 2 12 1)]
        times = filter (== "12:00 PM") (texts (shown (model' run)))
    times `shouldBe` []
    texts (shown (model' run)) `shouldContain` ["12:01 PM"]

  it "lists the latest chat first, with a draft where there is one" $ do
    let model = model' [Open (Chat bob), DraftChanged "for Bob" (at 1 0 0), Send (at 2 13 0), Open (Chat alice), DraftChanged "unsent" (at 1 0 0), Back]
        listed = texts (shown model)
    filter (`elem` ["Alice", "Bob"]) listed `shouldBe` ["Bob", "Alice"]
    listed `shouldContain` ["Draft: ", "unsent"]

  it "says Applied once the profile is" $ do
    texts (shown (model' [Open ProfileScreen])) `shouldContain` ["Applied"]
    texts (shown (model' [Open ProfileScreen, NicknameChanged "Toxer"]))
      `shouldContain` ["Apply changes"]

  it "says why an address isn't a Tox ID" $ do
    let model = model' [Open AddContact, AddressChanged "C0FFEE"]
    texts (shown model) `shouldContain` ["C0FFEE", "Must be 76 hexadecimal characters"]

  it "says why Tox wouldn't take a friend request" $ do
    let model = model' [Open AddContact, RequestChanged ""]
    texts (shown model) `shouldContain` ["The message can't be empty"]

  it "says why the app didn't start, with a button to try again" $ do
    let failed = startFailure (envTheme env) (Action 30) "Tox didn't start: PortInUse"
    texts failed `shouldContain` ["Tox didn't start: PortInUse"]
    [action | (covered, action) <- taps failed, "Try again" `elem` covered] `shouldBe` [Action 30]

  it "shows a notice in the chat until the user goes elsewhere" $ do
    let noticed = [Open (Chat alice), Notify "Calls need the microphone"]
    texts (shown (model' noticed)) `shouldContain` ["Calls need the microphone"]
    texts (shown (model' (noticed ++ [Back, Open (Chat alice)]))) `shouldNotContain` ["Calls need the microphone"]

  it "shows a contact's page, opened from its chat" $ do
    tapOn "Alice" (shown (model' [Open (Chat alice)])) `shouldBe` Just (T.Details alice)
    let page = shown (model' [ ClientEvent (Client.FriendStatusMessage alice "Writing Haskell"), ClientEvent (Client.FriendStatus alice Tox.Away)
                             , ClientEvent (Client.MuteChat alice (Just (at 2 18 0))), Open (Details alice) ])
        ContactId key = alice
    mapM_ (\t -> texts page `shouldContain` [t])
      ["Alice", "Writing Haskell", "Away", "Unmute notifications", "Muted until 6:00 PM", key, "Delete contact"]
    tapOn "Delete contact" page `shouldBe` Just (T.Delete alice)

  it "selects messages, and answers one" $ do
    let message n = messageAt alice n sample
        selected = shown (model' [Open (Chat alice), Select (message 0), Select (message 2)])
        replying = shown (model' [Open (Chat alice), Reply (message 0)])
    texts selected `shouldContain` ["2 selected"]
    tappedIn selected `shouldContain` [T.CopySelected, T.ForwardSelected]
    texts replying `shouldContain` ["> Hi! Is this the Haskell client?"]

  it "tells when a message went and came" $ do
    let info n = texts (shown (model' [Open (MessageInfo (Client.FriendChat alice) (messageAt alice n sample))]))
    info 1 `shouldContain` ["Sent", "Nov 1, 2026 3:15 PM", "Delivered", "Nov 1, 2026 3:16 PM"]
    info 0 `shouldContain` ["Received", "Nov 1, 2026 3:14 PM"]

  it "shows where time and memory go, opened last in the drawer" $ do
    last (tappedIn (shown (model' [OpenDrawer]))) `shouldBe` T.OpenDebug
    let screen = shown (model' [Open DebugScreen, ClientEvent (Client.Measured sampleReport)])
        page = texts screen
    tapOn "Export" screen `shouldBe` Just T.ExportDebug
    mapM_ (\line -> page `shouldContain` [line])
      [ "Database version 3", "Heap 20 MB after the last GC, at most 30 MB after a major one", "Render: 4.5 ms \x00d7\&3, worst 2.0 ms"
      , "tox main loop (blocked on BlockedOnSTM)", "Shared keys: 3000 of 2048", "UDP sent NodesRequest: 10, 20 KB" ]
    page `shouldNotContain` ["TCP relays: 0"]

  it "shows the settings, opened from the drawer" $ do
    tapOn "Settings" (shown (model' [OpenDrawer])) `shouldBe` Just T.OpenSettings
    let page = texts (shown (model' [Open SettingsScreen, ClientEvent (Client.SetAutoAccept 0), ClientEvent (Client.SetDevice (DeviceSettings True False True True Nothing))]))
    page `shouldContain` ["Accept files without asking", "Never"]
    page `shouldContain` ["Use IPv6", "Off"]

  it "says a contact hasn't answered our request yet" $ do
    let address = toxId "0123ABCD0123ABCD0123ABCD0123ABCD0123ABCD0123ABCD0123ABCD0123ABCD" "DEADBEEF"
    texts (shown (model' [Open AddContact, AddressChanged address, AddFriend, Back]))
      `shouldContain` ["0123ABCD", "Friend request sent"]

  it "shows a friend's file with its size, to take or decline" $ do
    let large = ClientEvent (Client.FileOffered alice (Receiving 0) (at 3 9 0) "notes.pdf" (autoAccepted + 1) "/received/notes.pdf" "id")
        i = messageAt alice 3 (model' [large])
        chat = shown (model' [large, Open (Chat alice)])
    texts chat `shouldContain` ["notes.pdf", "4.0 MB \x00B7 Offered"]
    tapOn "notes.pdf" chat `shouldBe` Just (T.TapMessage alice i)
    tapOn "Accept" chat `shouldBe` Just (T.TapMessage alice i)
    tapOn "Decline" chat `shouldBe` Just (T.CancelFile alice i)

  it "sends our file again once it wasn't sent, but not theirs" $ do
    let ours = map ClientEvent [Client.Stage alice (StagedFile "/sent/notes.pdf" "notes.pdf" 2048 "id"), Client.Send (Client.FriendChat alice) (at 3 9 0)]
        i = messageAt alice 3 (model' ours)
        chat events = shown (model' (events ++ [Open (Chat alice)]))
    tapOn "Send again" (chat ours) `shouldBe` Nothing
    texts (chat (ours ++ [ClientEvent (Client.Cancel alice i)])) `shouldContain` ["2.0 KB \x00B7 Not sent", "Send again"]
    tapOn "Send again" (chat (ours ++ [ClientEvent (Client.Cancel alice i)])) `shouldBe` Just (T.SendAgain alice i)
    let theirs = ClientEvent (Client.FileOffered alice (Receiving 0) (at 3 9 0) "b.pdf" (autoAccepted + 1) "/received/b.pdf" "b")
    tapOn "Send again" (chat [theirs, ClientEvent (Client.Cancel alice (messageAt alice 3 (model' [theirs])))]) `shouldBe` Nothing

  it "shows a call a linked device took, which can only be hung up here" $ do
    let talking = model' [ClientEvent (Client.CallInvited alice (at 3 9 0) True), ClientEvent (Client.AnswerCall (UTCTime (fromGregorian 2026 11 2) 43075) False)]
        client = modelClient talking
        relayed = shown talking { modelClient = client { clientCall = (\c -> c { callDevice = Just (ContactId "DD") }) <$> clientCall client } }
    texts relayed `shouldContain` ["On a linked device \x00B7 2:05"]
    tappedIn relayed `shouldBe` [T.HideCall, T.HangUp]

  it "shows a call over the screen, with how long it has gone" $ do
    let ringing = shown (model' [Open (Chat bob), ClientEvent (Client.CallInvited alice (at 3 9 0) False)])
    mapM_ (\t -> texts ringing `shouldContain` [t]) ["Alice", "Incoming voice call"]
    tappedIn ringing `shouldBe` [T.HangUp, T.Answer False, T.Answer True]
    let talking = shown (model' [ClientEvent (Client.CallInvited alice (at 3 9 0) False), ClientEvent (Client.AnswerCall (UTCTime (fromGregorian 2026 11 2) 43075) False)])
    texts talking `shouldContain` ["2:05"]
    let missed = shown (model' [Open (Chat alice), ClientEvent (Client.CallInvited alice (at 3 9 0) False), ClientEvent (Client.CallEnded alice (at 3 9 1))])
    mapM_ (\t -> texts missed `shouldContain` [t]) ["Missed voice call", "9:00 AM"]
    tappedIn talking `shouldBe` [T.HideCall, T.Route, T.Camera, T.Mute, T.HangUp]
