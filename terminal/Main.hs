{-# LANGUAGE OverloadedStrings #-}
-- | hTox in a terminal, with the icons as characters.
module Main (main) where

import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import           Hatter.Terminal      (TerminalConfig (..), runTerminal,
                                       terminalConfig)
import           Hatter.Widget        (ResourceName (..))
import           Tox.Client.Toxcore   (hsToxcore)
import           Tox.Client.Toxcore.C (cToxcore)

import           HTox.App             (app)

main :: IO ()
main = runTerminal (terminalConfig "htox") { terminalGlyph = (`Map.lookup` icons) . unResourceName } (app [hsToxcore, cToxcore])

icons :: Map.Map Text Text
icons = Map.fromList
  [ ("ic_add", "+")
  , ("ic_arrow_back", "\x2190")
  , ("ic_attach_file", "\x2295")
  , ("ic_bluetooth", "\x16d2")
  , ("ic_call", "\x260e")
  , ("ic_call_end", "\x2298")
  , ("ic_call_made", "\x2197")
  , ("ic_call_missed", "\x21af")
  , ("ic_call_received", "\x2199")
  , ("ic_chat_bubble_outline", "\x2709")
  , ("ic_check", "\x2713")
  , ("ic_check_box", "\x2611")
  , ("ic_check_box_outline_blank", "\x2610")
  , ("ic_close", "\x2715")
  , ("ic_content_copy", "\x29c9")
  , ("ic_delete", "\x232b")
  , ("ic_done", "\x2713")
  , ("ic_done_all", "\x2713\x2713")
  , ("ic_edit", "\x270e")
  , ("ic_exit_to_app", "\x21e5")
  , ("ic_flip_camera", "\x27f2")
  , ("ic_forward", "\x21aa")
  , ("ic_group", "\x2687")
  , ("ic_headset", "\x266b")
  , ("ic_image", "\x25a7")
  , ("ic_insert_drive_file", "\x25a4")
  , ("ic_keyboard_arrow_down", "\x2193")
  , ("ic_link", "\x221e")
  , ("ic_menu", "\x2630")
  , ("ic_mic", "\x2669")
  , ("ic_mic_off", "\x00d7")
  , ("ic_notifications", "\x237e")
  , ("ic_notifications_off", "\x2297")
  , ("ic_offline_bolt", "\x25cb")
  , ("ic_online_prediction", "\x25cf")
  , ("ic_radio_button_checked", "\x25c9")
  , ("ic_radio_button_unchecked", "\x25cb")
  , ("ic_reply", "\x21a9")
  , ("ic_schedule", "\x25f7")
  , ("ic_send", "\x27a4")
  , ("ic_videocam", "\x25a3")
  , ("ic_videocam_off", "\x25a2")
  , ("ic_volume_up", "\x266a")
  ]
