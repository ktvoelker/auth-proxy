
module Authenticate (app) where

import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types
import Network.Wai
import Network.Waitra
import Web.ClientSession (Key)

import qualified Config
import qualified HTML
import qualified Session

app :: Config.T -> Key -> Application
app c k = waitraMiddleware routes redirectToLogin
  where
    routes =
      [ simpleGet "/login" $ loginPage c k
      , simplePost "/login" $ login c k
      , simplePost "/verify" $ verify c k
      ]

redirectToLogin :: Application
redirectToLogin = const ($ responseLBS status303 [(hLocation, "/login")] "")

loginPage :: Config.T -> Key -> Application
loginPage c k req respond = do
  (session, setCookies) <-
    maybe (Session.new c k) (return . (, [])) $ Session.get c k req
  let
    args =
      [ ("__TITLE__", Config.authTitle c)
      , ("__TOKEN__", Session.token session)
      ]
  pageText <- HTML.login >>= HTML.render args
  respond . responseLBS status200 setCookies . fromStrict $ encodeUtf8 pageText

login :: Config.T -> Key -> Application
login = undefined

verify :: Config.T -> Key -> Application
verify = undefined

