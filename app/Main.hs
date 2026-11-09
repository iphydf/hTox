module Main (main) where

import           Foreign.Ptr          (Ptr)
import           Hatter               (startMobileApp)
import           Hatter.AppContext    (AppContext)
import           Tox.Client.Toxcore   (hsToxcore)
import           Tox.Client.Toxcore.C (cToxcore)

import           HTox.App             (app)

main :: IO (Ptr AppContext)
main = app [hsToxcore, cToxcore] >>= startMobileApp
