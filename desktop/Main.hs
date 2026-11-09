-- | hTox on the desktop, with Qt Quick.
module Main (main) where

import           Foreign.Ptr       (Ptr)
import           Hatter            (startMobileApp)
import           Hatter.AppContext (AppContext)
import           Hatter.Qt         (setResources)

import           HTox.App          (app)
import           Paths_htox_desktop (getDataDir)

main :: IO (Ptr AppContext)
main = do
  -- The same icons as on Android.
  setResources =<< getDataDir
  app >>= startMobileApp
