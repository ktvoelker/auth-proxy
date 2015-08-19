
module Main where

import Control.Lens
import Data.Monoid
import Network.HTTP.Types
import Network.Wai
import Network.Waitra
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import System.Environment
import System.Exit
import System.IO

import qualified Authenticate
import qualified Config
import qualified Proxy

warpSettings :: Config.T -> Warp.Settings
warpSettings conf =
  Warp.setPort (view Config.serverPort conf)
  $ Warp.setHost "127.0.0.1" Warp.defaultSettings

main :: IO ()
main = getArgs >>= \case
  [confPath] -> do
    conf <- Config.load confPath
    newApp conf >>= Warp.runSettings (warpSettings conf)
  _ -> do
    hPutStrLn stderr "Usage: auth-proxy CONFIG_FILE"
    exitFailure

corsPolicy :: Config.T -> Request -> Maybe CorsResourcePolicy
corsPolicy conf =
  const . Just
  $ CorsResourcePolicy
    { corsOrigins = Just (view Config.serverAllowOrigins conf, True)
    , corsMethods =
      [ methodGet
      , methodPost
      , methodHead
      , methodPut
      , methodDelete
      , methodOptions
      , methodPatch
      ]
    , corsRequestHeaders = [hContentType]
    , corsExposedHeaders = Just [hContentType]
    , corsMaxAge = Just 3600
    , corsVaryOrigin = True
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

newApp :: Config.T -> IO Application
newApp conf =
  logger
  . cors (corsPolicy conf)
  . waitraMiddleware (routes <> Authenticate.routes conf)
  <$> Proxy.newApp conf 
  where
    logger = if view Config.debug conf then logStdoutDev else logStdout

routes :: [Route]
routes =
  [ simpleGet "/favicon.ico" $ const ($ responseLBS status404 [] "")
  ]

