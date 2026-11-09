{-# LANGUAGE StrictData #-}
module HTox.PlaybackSpec (spec) where

import           Control.Exception    (ErrorCall (..), throwIO, try)
import           Data.IORef           (modifyIORef', newIORef, readIORef)
import           Data.Maybe           (fromMaybe)
import qualified Data.Vector.Storable as V
import           Test.Hspec

import           HTox.Playback
import           Tox.AV               (AudioFrame (..))
import qualified Tox.Core.Lanes       as Lanes

-- | A device on a clock of its own, which plays what it's given from the
-- first block on, says where it is in steps of 20 ms, as Android's tracks
-- do, and is heard 50 ms after.
data Device = Device
  { clock   :: Double -- ^ Seconds.
  , started :: Maybe Double
  , written :: Double -- ^ Ms.
  }

played, position :: Device -> Double
played d = maybe 0 (\t -> min (written d) ((clock d - t) * 1000)) (started d)
position d = 20 * fromIntegral (floor (played d / 20) :: Int)

spec :: Spec
spec =
  it "pulls each block for when it'll be heard, though the device says where it is in steps" $ do
    lanes <- Lanes.newLanes
    device <- newIORef (Device 0 Nothing 0)
    heardAt <- newIORef []
    let output = Output
          { outPlay = \_ -> do
              modifyIORef' device $ \d -> d { started = Just (fromMaybe (clock d) (started d)), written = written d + 10 }
              (\d -> round (written d - position d)) <$> readIORef device
          , outLatency = (\d -> round (position d - played d + 50)) <$> readIORef device
          , outClock = clock <$> readIORef device
          , outSleep = \us -> modifyIORef' device $ \d -> d { clock = clock d + fromIntegral us / 1000000 }
          }
        pull ahead = do
          now <- clock <$> readIORef device
          modifyIORef' heardAt ((now * 1000 + fromIntegral ahead) :)
          pulled <- length <$> readIORef heardAt
          if pulled > 100 then throwIO (ErrorCall "enough") else pure (Just (AudioFrame 48000 1 (V.replicate 480 0)))
    _ <- try (pumpAudio lanes output pull) :: IO (Either ErrorCall ())
    -- Until the first block plays, the device's latency isn't known.
    times <- drop 2 . reverse <$> readIORef heardAt
    zipWith subtract times (drop 1 times) `shouldSatisfy` all (\step -> abs (step - 10) <= 1)
