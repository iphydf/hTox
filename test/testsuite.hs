module Main (main) where

import           Test.Hspec

import qualified HTox.EmojiSpec
import qualified HTox.LinksSpec
import qualified HTox.ModelSpec
import qualified HTox.PlaybackSpec
import qualified HTox.StoreSpec
import qualified HTox.ViewSpec

main :: IO ()
main = hspec $ do
  describe "Emoji" HTox.EmojiSpec.spec
  describe "Links" HTox.LinksSpec.spec
  describe "Model" HTox.ModelSpec.spec
  describe "Playback" HTox.PlaybackSpec.spec
  describe "Store" HTox.StoreSpec.spec
  describe "View" HTox.ViewSpec.spec
