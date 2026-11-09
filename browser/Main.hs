-- | hTox in a browser page, reaching Tox's relays through a WebSocket
-- gateway.
module Main (main) where

import           Hatter.Browser     (runBrowser)
import           Tox.Client.Toxcore (hsToxcore)

import           HTox.App           (app)

foreign export javascript "start" start :: IO ()

-- | TokTok's gateway, unless the page names one.
foreign import javascript unsafe "if (globalThis.toxGateway === undefined) globalThis.toxGateway = 'wss://tox-gateway.onrender.com'"
  js_defaultGateway :: IO ()

start :: IO ()
start = js_defaultGateway >> runBrowser (app [hsToxcore])

-- | The page starts it with 'start'.
main :: IO ()
main = pure ()
