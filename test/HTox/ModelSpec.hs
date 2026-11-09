{-# LANGUAGE OverloadedStrings #-}
module HTox.ModelSpec (spec) where

import qualified Data.Map.Strict     as Map
import           Data.Maybe          (listToMaybe)
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import           Test.Hspec

import qualified Tox.Application.Tox as Tox
import qualified Tox.Client.Update   as Client

import           HTox.Model
import           HTox.Sample

messagesOf :: ContactId -> Model -> [Message]
messagesOf cid = maybe [] contactMessages . Map.lookup cid . clientContacts . modelClient

-- | Commands of the last event.
commands :: [Event] -> Event -> [Command]
commands events = snd . flip update (applying events sample)

groupOf :: Model -> Maybe Group
groupOf = Map.lookup team . clientGroups . modelClient

dana :: Member
dana = Member (Text.replicate 8 "DA7A0000") "Dana"

spec :: Spec
spec = do
  it "measures as the debug screen opens and each second while it's open" $ do
    commands [] (Open DebugScreen) `shouldBe` [MeasureDebug]
    commands [Open DebugScreen] (ClientEvent (Client.Tick (at 3 9 0))) `shouldBe` [MeasureDebug]
    commands [] (ClientEvent (Client.Tick (at 3 9 0))) `shouldBe` []

  it "tells a friend we're typing as their chat's draft changes, but no group" $ do
    commands [Open (Chat alice)] (DraftChanged "h" (at 3 9 0)) `shouldContain` [Client.SetTyping alice True]
    commands [Open (Chat alice)] (DraftChanged " " (at 3 9 0)) `shouldBe` []
    commands [Open (GroupChat team)] (DraftChanged "h" (at 3 9 0)) `shouldBe` []

  it "drops the notice when the profiles change" $
    modelNotice (applying [Open ProfilesScreen, Notify "hTox has those profiles already", ProfilesChanged []] sample)
      `shouldBe` Nothing

  it "notifies of a muted group's messages naming us only, unless asked not to" $ do
    let events = [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
                 , ClientEvent (Client.MemberJoined team (at 3 9 0) 1 dana), ClientEvent (Client.GroupConnected team (at 3 9 0)), Back
                 , ClientEvent (Client.MuteGroup team Nothing)
                 , ClientEvent (Client.GroupReceived team (at 3 9 1) 1 Tox.MessageNormal "lunch?" Nothing)
                 , ClientEvent (Client.GroupReceived team (at 3 9 2) 1 Tox.MessageNormal "tox user, lunch?" Nothing) ]
        shown evs = Map.lookup team (groupNotifications (at 3 9 3) (modelClient (applying evs sample)))
    shown events `shouldBe` Just ["Dana: tox user, lunch?"]
    shown (events ++ [ClientEvent (Client.ToggleMentions team)]) `shouldBe` Nothing

  it "shows a relay bot's messages under the names it relays, once chosen" $ do
    let bridge = Member (Text.replicate 8 "B0B00000") "Bridge"
        events = [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
                 , ClientEvent (Client.MemberJoined team (at 3 9 0) 1 dana), ClientEvent (Client.MemberJoined team (at 3 9 0) 2 bridge)
                 , ClientEvent (Client.GroupConnected team (at 3 9 0)), Back, ClientEvent (Client.MuteGroup team Nothing)
                 , ClientEvent (Client.GroupReceived team (at 3 9 1) 2 Tox.MessageNormal "<Bob> hi" Nothing)
                 , ClientEvent (Client.GroupReceived team (at 3 9 2) 1 Tox.MessageNormal "<Dana> is me" Nothing)
                 , ClientEvent (Client.GroupReceived team (at 3 9 3) 2 Tox.MessageNormal "<Carol> tox user, lunch?" Nothing) ]
        relaying = applying (events ++ [ClientEvent (Client.ChooseRelayBot team (Just (memberKey bridge)))]) sample
        g = groupOf relaying
    fmap relayCounts g `shouldBe` Just [(bridge, 2, 2), (dana, 1, 1)]
    fmap (relayedThrough (clientProfile (modelClient relaying))) g `shouldBe` Just [("Carol", at 3 9 3), ("Bob", at 3 9 1)]
    Map.lookup team (groupNotifications (at 3 9 4) (modelClient relaying)) `shouldBe` Just ["Carol via Bridge: tox user, lunch?"]
    Map.lookup team (groupNotifications (at 3 9 4) (modelClient (applying events sample)))
      `shouldBe` Just ["Bridge: <Carol> tox user, lunch?"]

  it "applies the founder's settings when they make sense" $ do
    let settings = Tox.GroupSettings True "" 100 True Tox.VoiceAll
        opened = [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
                 , ClientEvent (Client.GroupConnected team (at 3 9 0)), ClientEvent (Client.SettingsKnown team settings), Open (GroupSettingsScreen team) ]
    modelPeerLimit (applying opened sample) `shouldBe` "100"
    commands (opened ++ [PeerLimitEdited "50", GroupPasswordEdited "pw"]) ApplyGroupSettings
      `shouldBe` [SetGroupSettings team settings { Tox.groupPeerLimit = 50, Tox.groupPassword = "pw" }]
    commands (opened ++ [PeerLimitEdited "0"]) ApplyGroupSettings `shouldBe` []

  it "counts what is received while its chat isn't in front of the user" $ do
    let unread events = contactUnread <$> Map.lookup alice (clientContacts (modelClient (applying events sample)))
        more = ClientEvent (Client.Received alice (at 3 9 0) Tox.MessageNormal "more")
    unread [] `shouldBe` Just 1
    unread [more] `shouldBe` Just 2
    unread [Open (Chat alice)] `shouldBe` Just 0
    unread [Shown True, Open (Chat alice), more] `shouldBe` Just 0
    unread [Shown True, Open (Chat alice), Shown False, more] `shouldBe` Just 1
    unread [Open (Chat alice), more, Shown True] `shouldBe` Just 0
    unread [Shown True, Open (Details alice), more, Back] `shouldBe` Just 0

  it "notifies of each contact's unread messages" $ do
    let notified events = Map.lookup alice (notifications (at 3 9 0) (modelClient (applying events sample)))
    notified [ClientEvent (Client.Received alice (at 3 9 0) Tox.MessageNormal "more")] `shouldBe` Just ["Looks good already.", "more"]
    let answered = [ ClientEvent (Client.CallInvited alice (at 3 9 0) False), ClientEvent (Client.AnswerCall (at 3 9 0) False), ClientEvent (Client.HangUp (at 3 9 1))
                   , ClientEvent (Client.Received alice (at 3 9 2) Tox.MessageNormal "more") ]
    map messageCall (messagesOf alice (applying answered sample))
      `shouldSatisfy` elem (Just (CallRecord False (Just 60) False))
    notified answered `shouldBe` Just ["Looks good already.", "more"]
    notified [Open (Chat alice)] `shouldBe` Nothing

  it "applies only the profile Tox takes, counting bytes" $ do
    let tooLong = Text.replicate 33 "\x1F600"
        (model, applied) = update ApplyProfile (applying [Open ProfileScreen, NicknameChanged tooLong] sample)
    profileNickname (clientProfile (modelClient model)) `shouldBe` profileNickname (clientProfile (modelClient sample))
    applied `shouldBe` []
    profileError (Text.replicate 32 "\x1F600") "" `shouldBe` Nothing

  it "starts a video call on the loudspeaker and follows the platform's routes" $ do
    let route = ((,) <$> callRoute <*> callRoutes) . modelPhoneCall . (`applying` sample)
    route [Open (Chat alice), StartCall (at 3 9 0) True] `shouldBe` (Speaker, [Earpiece, Speaker])
    route [ClientEvent (Client.CallInvited alice (at 3 9 0) False)] `shouldBe` (Earpiece, [Earpiece, Speaker])
    route [ClientEvent (Client.CallInvited alice (at 3 9 0) False), RoutesChanged Bluetooth [Earpiece, Speaker, Bluetooth]]
      `shouldBe` (Bluetooth, [Earpiece, Speaker, Bluetooth])

  it "hides a call going on behind a banner, but not one ringing" $ do
    let talking = [Open (Chat alice), StartCall (at 3 9 0) False, ClientEvent (Client.CallPeer alice (at 3 9 0) False)]
        hidden events = callHidden (modelPhoneCall (applying events sample))
    hidden (talking ++ [Back]) `shouldBe` True
    modelScreen (applying (talking ++ [Back]) sample) `shouldBe` Chat alice
    hidden (talking ++ [HideCall, ShowCall]) `shouldBe` False
    back (applying [ClientEvent (Client.CallInvited alice (at 3 9 0) False)] sample) `shouldBe` Nothing

  it "waits on its chats, with the call hidden, while another profile is shown" $ do
    let left = applying [Open (Chat alice), StartCall (at 3 9 0) False, ClientEvent (Client.CallPeer alice (at 3 9 0) False), Open ProfilesScreen, Leave] sample
    (modelScreen left, callHidden (modelPhoneCall left)) `shouldBe` (Contacts, True)

  it "takes a proxy only with a host and a port" $ do
    let device events = settingProxy (clientDevice (modelClient (applying (Open SettingsScreen : Open ProxyScreen : events) sample)))
        socks = [ProxyKindChosen (Just Tox.Socks5Proxy), ProxyHostChanged " 127.0.0.1 "]
    device (socks ++ [ProxyPortChanged "9050", ApplyProxy]) `shouldBe` Just (Tox.Proxy Tox.Socks5Proxy "127.0.0.1" 9050)
    device (socks ++ [ProxyPortChanged "70000", ApplyProxy]) `shouldBe` Nothing
    modelNotice (applying (Open ProxyScreen : socks ++ [ApplyProxy]) sample) `shouldBe` Just "The port is a number from 1 to 65535"

  it "keeps each chat's draft" $ do
    let model = applying [Open (Chat alice), DraftChanged "for Alice" (at 1 0 0), Back, Open (Chat bob)] sample
    Map.lookup alice (clientDrafts (modelClient model)) `shouldBe` Just "for Alice"
    Map.lookup bob (clientDrafts (modelClient model)) `shouldBe` Nothing

  it "orders the contacts by their latest message, the rest by name" $ do
    map fst (contactOrder (modelClient sample)) `shouldBe` [alice, bob]
    map fst (contactOrder (modelClient (applying [ClientEvent (Client.Received bob (at 3 9 0) Tox.MessageNormal "now")] sample))) `shouldBe` [bob, alice]

  it "notifies of missed calls apart from messages" $ do
    let missed = applying [ClientEvent (Client.CallInvited alice (at 3 9 0) False), ClientEvent (Client.CallEnded alice (at 3 9 1))] sample
    Map.lookup alice (notifications (at 3 9 2) (modelClient missed)) `shouldBe` Just ["Looks good already."]
    Map.lookup alice (missedCalls (at 3 9 2) (modelClient missed)) `shouldBe` Just 1
    Map.lookup alice (missedCalls (at 3 9 2) (modelClient sample)) `shouldBe` Nothing

  it "sums for the other profiles what is unread in chats that aren't muted" $ do
    let unread events = summaryUnread (profileSummary (at 3 9 0) (modelClient (applying events sample)))
        more = ClientEvent (Client.Received alice (at 3 9 0) Tox.MessageNormal "more")
        -- Alice's one unread message, and the other chats' unread ones.
        earlier = unread []
    unread [more] `shouldBe` earlier + 1
    unread [more, ClientEvent (Client.MuteChat alice Nothing)] `shouldBe` earlier - 1

  it "notifies of no muted chat until its mute ends" $ do
    let mutedUntil end = applying [ClientEvent (Client.MuteChat alice end)] sample
    Map.member alice (notifications (at 2 12 0) (modelClient (mutedUntil (Just (at 2 13 0))))) `shouldBe` False
    Map.member alice (notifications (at 2 14 0) (modelClient (mutedUntil (Just (at 2 13 0))))) `shouldBe` True
    Map.member alice (notifications (at 9 0 0) (modelClient (mutedUntil Nothing))) `shouldBe` False
    Map.member alice (notifications (at 2 12 0) (modelClient (applying [ClientEvent (Client.UnmuteChat alice)] (mutedUntil Nothing)))) `shouldBe` True

  it "keeps only the newest of a chat the user left" $ do
    let many = [ClientEvent (Client.Received alice (at 4 0 n) Tox.MessageNormal "more") | n <- [1 .. fromIntegral chatPage + 5]]
        left = applying (Open (Chat alice) : many ++ [Back]) sample
    (length (messagesOf alice left), contactEarlier <$> Map.lookup alice (clientContacts (modelClient left)))
      `shouldBe` (chatPage, Just True)
    pageBefore (applying [Open (Chat alice)] left) `shouldBe` (,) (Client.FriendChat alice) . messageId <$> listToMaybe (messagesOf alice left)
    pageBefore (applying [Open (Chat alice)] sample) `shouldBe` Nothing

  it "goes back from a contact's page to its chat" $
    modelScreen <$> back (applying [Open (Details alice)] sample) `shouldBe` Just (Chat alice)

  it "selects messages until Back, which leaves the chat only then" $ do
    let message n = messageAt alice n sample
        selected = applying [Open (Chat alice), Select (message 0), Select (message 2), Select (message 0)] sample
    modelSelected selected `shouldBe` Set.fromList [message 2]
    ((\m -> (modelScreen m, modelSelected m)) <$> back selected) `shouldBe` Just (Chat alice, Set.empty)

  it "copies one message as its text, several with whose they were" $ do
    let message n = messageAt alice n sample
    copiedText (modelClient sample) (Client.FriendChat alice) [message 1] `shouldBe` "It is, and it's on the network now."
    copiedText (modelClient sample) (Client.FriendChat alice) [message 1, message 0] `shouldBe` "Alice: Hi! Is this the Haskell client?\nTox User: It is, and it's on the network now."

  it "counts what comes while the chat is scrolled away, until it is back" $ do
    let away = [Open (Chat alice), ScrolledAway True, ClientEvent (Client.Received alice (at 3 9 0) Tox.MessageNormal "one")]
    modelUnseen (applying away sample) `shouldBe` 1
    modelUnseen (applying (away ++ [ScrolledAway False]) sample) `shouldBe` 0
    modelJump (applying (away ++ [JumpToEnd]) sample) `shouldBe` modelJump sample + 1

  it "sends no request Tox would refuse" $ do
    let events = [Open AddContact, AddressChanged (toxId (Text.replicate 8 "0123ABCD") "DEADBEEF")]
    commands (events ++ [RequestChanged "   "]) AddFriend `shouldBe` []
    requestError (Text.replicate 921 "a") `shouldBe` Nothing
    requestError (Text.replicate 922 "a") `shouldBe` Just "The message is too long"

  it "adds nothing that isn't a Tox ID" $ do
    let events = [Open AddContact, AddressChanged "C0FFEE"]
    clientContacts (modelClient (applying (events ++ [AddFriend]) sample)) `shouldBe` clientContacts (modelClient sample)
    commands events AddFriend `shouldBe` []

  it "finds a typo by the checksum" $ do
    let address = toxId (Text.replicate 8 "0123ABCD") "DEADBEEF"
        typo = Text.take 10 address <> "F" <> Text.drop 11 address
    addressError "" address `shouldBe` Nothing
    addressError "" typo `shouldBe` Just "Not a valid Tox ID, please check it for typos"

  it "adds no contact of ourselves" $ do
    let ours = profileToxId (clientProfile (modelClient sample))
    addressError ours (Text.toLower ours) `shouldBe` Just "This is your own Tox ID"

  it "keeps profile edits until they are applied" $ do
    let events = [Open ProfileScreen, NicknameChanged "Toxer"]
    profileNickname (clientProfile (modelClient (applying events sample))) `shouldBe` "Tox User"
    profileNickname (clientProfile (modelClient (applying (events ++ [ApplyProfile]) sample))) `shouldBe` "Toxer"
    commands events ApplyProfile `shouldBe` [SetProfile (clientProfile (modelClient sample)) { profileNickname = "Toxer" }]

  it "goes back from the drawer, then the screen, then out" $ do
    modelDrawer <$> back (applying [OpenDrawer] sample) `shouldBe` Just False
    modelScreen <$> back (applying [Open ProfileScreen] sample) `shouldBe` Just Contacts
    back sample `shouldBe` Nothing

  it "notifies of a group's messages by their sender" $ do
    let events =
          [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
          , ClientEvent (Client.MemberJoined team (at 3 9 0) 1 dana), ClientEvent (Client.GroupConnected team (at 3 9 0))
          , ClientEvent (Client.GroupReceived team (at 3 9 2) 1 Tox.MessageNormal "hi" Nothing), Back ]
    Map.lookup team (groupNotifications (at 3 9 4) (modelClient (applying events sample))) `shouldBe` Just ["Dana: hi"]

  it "founds a named group and opens it, and joins only by a whole chat ID" $ do
    modelNotice (applying [Open NewGroupScreen, Found] sample) `shouldBe` Just "The name can't be empty"
    let named = [Open NewGroupScreen, FoundingChosen PublicGroup, GroupNameEdited " Lab "]
        created = applying (named ++ [Found, ClientEvent (Client.GroupCreated team NgcGroup "Lab" (at 3 9 0))]) sample
    commands named Found `shouldBe` [FoundGroup PublicGroup "Lab" "Tox User"]
    modelScreen created `shouldBe` GroupChat team
    commands [Open JoinGroupScreen, ChatIdEdited (Text.replicate 8 "c0ffee00"), PasswordChanged "pw"] JoinPublic
      `shouldBe` [JoinPublicGroup team "Tox User" "pw"]
    commands [Open JoinGroupScreen, ChatIdEdited "C0FFEE"] JoinPublic `shouldBe` []

  it "says things to the member chosen on the group page, and notifies of theirs" $ do
    let inGroup = [ ClientEvent (Client.Invited alice team NgcGroup "Team" "invite"), AnswerInvite alice team True
                  , ClientEvent (Client.MemberJoined team (at 3 9 0) 1 dana), ClientEvent (Client.GroupConnected team (at 3 9 0)) ]
        -- Like the member menu on the group page.
        written = inGroup ++ [Open (GroupDetails team), PrivatelyTo (Just (memberKey dana)), Open (GroupChat team), DraftChanged "psst" (at 1 0 0)]
    commands written (Send (at 3 9 1)) `shouldBe` [SendPrivate team (entryAt team 0 (applying (written ++ [Send (at 3 9 1)]) sample)) 1 "psst"]
    let heard = applying (inGroup ++ [Back, ClientEvent (Client.MuteGroup team Nothing)
                                     , ClientEvent (Client.PrivateReceived team (at 3 9 2) 1 Tox.MessageNormal "hush")]) sample
    -- Even when muted (it's to us alone).
    Map.lookup team (groupNotifications (at 3 9 3) (modelClient heard)) `shouldBe` Just ["Dana (privately): hush"]
    let gone = applying (written ++ [ClientEvent (Client.MemberLeft team (at 3 9 1) 1 Tox.ExitQuit)]) sample
    modelNotice (fst (update (Send (at 3 9 1)) gone)) `shouldBe` Just "They have left the group, so nothing was sent"

  it "says another profile has the call, and counts the one missed" $ do
    let missed = applying [ClientEvent (Client.CallInvited alice (at 3 9 0) False), ClientEvent (Client.CallElsewhere (at 3 9 0))] sample
    missedCalls (at 3 9 0) (modelClient missed) Map.!? alice `shouldBe` Just 1
    modelNotice (applying [Open (Chat alice), StartCall (at 3 9 0) False, ClientEvent (Client.CallElsewhere (at 3 9 0))] sample)
      `shouldBe` Just "Another profile has a call"

  it "previews calls, actions and places" $ do
    let talked = [Open (Chat alice), StartCall (at 3 9 0) True, ClientEvent (Client.CallPeer alice (at 3 9 0) True), ClientEvent (Client.HangUp (at 3 9 2))]
        missed = [ClientEvent (Client.CallInvited alice (at 3 9 0) False), ClientEvent (Client.CallEnded alice (at 3 9 1))]
    map (preview "Alice") (filter ((/= Nothing) . messageCall) (messagesOf alice (applying (talked ++ missed) sample)))
      `shouldBe` ["Outgoing video call \x00B7 2:00", "Missed voice call"]
    let waved = applying [ClientEvent (Client.Received alice (at 3 9 0) Tox.MessageAction "waves")] sample
    Map.lookup alice (notifications (at 3 9 2) (modelClient waved)) `shouldBe` Just ["Looks good already.", "* Alice waves"]
    let shared = [Open (Chat alice), ShareLocation 52.37 4.9, Send (at 3 9 0)]
    commands (init shared) (Send (at 3 9 0)) `shouldBe` [SendMessage alice (messageAt alice 3 (applying shared sample)) "geo:52.370000,4.900000" 0]
    map (preview "Alice") (drop 3 (messagesOf alice (applying shared sample))) `shouldBe` ["\x1F4CD Location"]

  it "forwards to the chosen contact and opens its chat" $
    modelScreen (applying [Open (Forward (Client.FriendChat alice) [messageAt alice 0 sample]), ForwardTo bob (at 3 9 0)] sample) `shouldBe` Chat bob

  it "adds a contact by Tox ID with our request, and opens its chat" $ do
    let key = Text.replicate 8 "0123ABCD"
        address = toxId key "DEADBEEF"
        events = [Open AddContact, AddressChanged ("tox:" <> address)]
        model = applying (events ++ [AddFriend]) sample
    (modelScreen model, modelAddress model) `shouldBe` (Chat (ContactId key), "")
    commands events AddFriend `shouldBe` [RequestFriend address "Hey, I'm Tox User, come chat with me on Tox!"]

  it "goes back to the form when Tox refuses a contact, saying why" $ do
    let address = toxId (Text.replicate 8 "0123ABCD") "DEADBEEF"
        model = applying [Open AddContact, AddressChanged address, AddFriend, ClientEvent (Client.AddFailed address (Just Tox.AlreadyFriend))] sample
    (modelScreen model, modelAddress model, modelAddError model) `shouldBe` (AddContact, address, Just "Already a contact")
