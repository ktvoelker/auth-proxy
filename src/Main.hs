
module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Authenticate
import qualified Config
import HTML
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Proxy
import qualified Session

main :: IO ()
main = getArgs >>= \case
  [configPath] -> do
    config <- Config.load configPath
    Warp.run (Config.serverPort config) $ app config
  _ -> do
    hPutStrLn stderr "Usage: auth-proxy CONFIG_FILE"
    exitFailure

app :: Config.T -> Application
app config req =
  if Session.authenticated config req
  then Proxy.app config req
  else Authenticate.app config req

