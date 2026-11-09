{-# LANGUAGE OverloadedStrings #-}
-- | A model with friends and chats, made from events.
module HTox.Sample
  ( alice
  , bob
  , carol
  , team
  , at
  , applying
  , messageAt
  , entryAt
  , history
  , sample
  , sampleReport
  ) where

import qualified Data.Text                   as Text
import           Data.Time                   (UTCTime (..), fromGregorian,
                                              secondsToDiffTime)

import qualified Data.Map.Strict             as Map
import qualified Tox.Application.Tox         as Tox
import qualified Tox.AV                      as AV
import qualified Tox.AV.Playout              as Playout
import           Tox.Client                  (initialClient)
import qualified Tox.Client.Debug            as Debug
import           Tox.Client.Types            (unused)
import qualified Tox.Client.Update           as Client
import qualified Tox.Network.Core.PacketKind as PacketKind

import           HTox.Model

alice, bob, carol :: ContactId
alice = ContactId (Text.replicate 8 "3B8EEA1F")
bob = ContactId (Text.replicate 8 "E8743B05")
carol = ContactId (Text.replicate 8 "19A974C2")

team :: GroupId
team = GroupId (Text.replicate 8 "C0FFEE00")

-- | In November 2026.
at :: Int -> Integer -> Integer -> UTCTime
at day hour minute = UTCTime (fromGregorian 2026 11 day)
  (secondsToDiffTime (hour * 3600 + minute * 60))

applying :: [Event] -> Model -> Model
applying events model = foldl (\m e -> fst (update e m)) model events

-- | IDs in the order the sample's client makes them.
made :: Int -> MessageId
made = (unused (clientIds initialClient) !!)

messageAt :: ContactId -> Int -> Model -> MessageId
messageAt cid n = messageId . (!! n) . maybe [] contactMessages . Map.lookup cid . clientContacts . modelClient

entryAt :: GroupId -> Int -> Model -> MessageId
entryAt gid n = entryId . (!! n) . maybe [] groupEntries . Map.lookup gid . clientGroups . modelClient

-- | Alice online with a chat, Bob offline, and a request from Carol.
sample :: Model
sample = applying history initial

history :: [Event]
history =
  [ ClientEvent (Client.Started (toxId (Text.replicate 8 "5A17ED00") "0BADF00D"))
  , ClientEvent (Client.Requested alice "Hi, it's Alice")
  , ClientEvent (Client.AcceptRequest alice)
  , ClientEvent (Client.FriendNamed alice "Alice")
  , ClientEvent (Client.FriendOnline alice True)
  , ClientEvent (Client.Received alice (at 1 15 14) Tox.MessageNormal "Hi! Is this the Haskell client?")
  , Open (Chat alice)
  , DraftChanged "It is, and it's on the network now." (at 1 0 0)
  , Send (at 1 15 15)
  , ClientEvent (Client.Confirmed alice (made 1) 1 (at 1 15 16))
  , Back
  , ClientEvent (Client.Received alice (at 2 9 2) Tox.MessageNormal "Looks good already.")
  , ClientEvent (Client.Requested bob "Hi, it's Bob")
  , ClientEvent (Client.AcceptRequest bob)
  , ClientEvent (Client.FriendNamed bob "Bob")
  , ClientEvent (Client.Requested carol "Hey, I'm Carol, come chat with me on Tox!")
  ]

-- | A measurement of a busy instance, for the debug screen and export.
sampleReport :: Debug.Report
sampleReport = Debug.Report
  { Debug.reportRuntime = Just Debug.Runtime
      { Debug.rtLiveBytes = 20 * 1048576, Debug.rtMaxLive = 30 * 1048576
      , Debug.rtGcs = 120, Debug.rtMajorGcs = 4
      , Debug.rtGcCpu = 50000000, Debug.rtMutatorCpu = 950000000
      , Debug.rtGcElapsed = 60000000, Debug.rtElapsed = 9000000000
      , Debug.rtAllocated = 500 * 1048576, Debug.rtCopied = 40 * 1048576
      , Debug.rtMemInUse = 48 * 1048576, Debug.rtLarge = 6 * 1048576, Debug.rtFragmented = 1048576
      }
  , Debug.reportThreads = Map.fromList [(("tox main loop", "blocked on BlockedOnSTM"), 1)]
  , Debug.reportApp = Map.fromList [("Render", Tox.Lane 3 4500000 2000000)]
  , Debug.reportTox = Map.fromList [("Periodic work", Tox.Lane 20 9000000 1000000)]
  , Debug.reportSizes = [Tox.Size "Shared keys" 3000 (Just 2048), Tox.Size "TCP relays" 0 Nothing]
  , Debug.reportTraffic =
      [ (Tox.Packets Tox.Sent Tox.Udp (PacketKind.kindToByte PacketKind.NodesRequest), Tox.Count 10 20480)
      , (Tox.RelayLink Tox.Sent, Tox.Count 2 3072)
      , (Tox.Requests Tox.SearchFriend, Tox.Count 7 1400)
      ]
  , Debug.reportCalls =
      [ Debug.Call "1d16d2ab" (Playout.Stats 3 1 2 0) (Just (AV.VideoStats 600 5 7 22 2 3 66 598 0))
          (Just "c2.exynos.h264.encoder, NV12 480x640 in rows of 512, planes of 640") ]
  }
