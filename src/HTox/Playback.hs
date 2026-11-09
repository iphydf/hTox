{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | Playing a call's audio as the device takes it.
module HTox.Playback
  ( Output (..)
  , pumpAudio
  ) where

import           Control.Monad        (when)
import qualified Data.Vector.Storable as V

import           Tox.AV               (AudioFrame (..))
import qualified Tox.Core.Lanes       as Lanes
import           Tox.Core.Lanes       (Lanes)

-- | Where a call's audio plays.
data Output = Output
  { outPlay    :: AudioFrame -> IO Int
    -- ^ Plays a block, and says how many ms of audio wait to be played, it
    -- included.
  , outLatency :: IO Int -- ^ How many ms after it's played audio is heard.
  , outClock   :: IO Double -- ^ Seconds.
  , outSleep   :: Int -> IO () -- ^ Microseconds.
  }

-- | Keeps about 30 ms of a call's audio waiting to be played, each block
-- pulled for when it'll be heard. Between writes, what waits shrinks as time
-- passes (sleeps may take longer than asked).
pumpAudio :: Lanes -> Output -> (Int -> IO (Maybe AudioFrame)) -> IO ()
pumpAudio lanes out pull = outClock out >>= go 0 0
  where
    go written latency at = do
      now <- outClock out
      let waiting = max 0 (written - round ((now - at) * 1000))
      if waiting >= 30 then outSleep out 5000 >> go written latency at else
        pull (waiting + latency) >>= \case
          -- Before the audio starts, nothing waits.
          Nothing -> outSleep out 10000 >> go 0 latency now
          Just frame@(AudioFrame rate channels pcm) -> do
            waiting' <- Lanes.timed lanes "Call audio playback" (outPlay out frame)
            -- Read right after what waits: the device's position moves in
            -- steps, so only the two taken together say when it's heard.
            latency' <- outLatency out
            -- How far behind the audio plays, as a lane for its worst and average.
            Lanes.took lanes "Call audio behind" (fromIntegral (waiting' + latency') * 1000000)
            -- Where nothing plays audio, nothing waits: pull it as fast as it
            -- would play.
            when (waiting' <= 0) $ outSleep out (V.length pcm `div` channels * 1000000 `div` rate)
            outClock out >>= go waiting' latency'
