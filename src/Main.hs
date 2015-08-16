
module Main where

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
app = waitraMiddleware routes . authApp

authApp :: Config.T -> Application
authApp config req respond = do
  if Session.authenticated config req
  then Proxy.app config req respond
  else Authenticate.app config req respond

routes :: [Route]
routes =
  [ simpleGet "/favicon.ico" $ const ($ responseLBS status404 [] "")
  ]

