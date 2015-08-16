
module Main where

import Control.Lens
import Data.Monoid
import Network.HTTP.Types
import Network.Wai
import Network.Waitra
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment
import System.Exit
import System.IO

import qualified Authenticate
import qualified Config
import qualified Proxy

main :: IO ()
main = getArgs >>= \case
  [configPath] -> do
    config <- Config.load configPath
    Warp.run (view Config.serverPort config) $ app config
  _ -> do
    hPutStrLn stderr "Usage: auth-proxy CONFIG_FILE"
    exitFailure

app :: Config.T -> Application
app config = waitraMiddleware (routes <> Authenticate.routes config) (Proxy.app config)

routes :: [Route]
routes =
  [ simpleGet "/favicon.ico" $ const ($ responseLBS status404 [] "")
  ]

