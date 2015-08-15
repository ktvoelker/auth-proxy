
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
import qualified Token

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
  session <- maybe Session.new return $ Session.get c k req
  token <- Token.new
  let session' = session { Session.token = Just token }
  setCookie <- Session.setCookie c k session'
  let
    args =
      [ ("__TITLE__", Config.authTitle c)
      , ("__TOKEN__", token)
      ]
  pageText <- HTML.login >>= HTML.render args
  respond . responseLBS status200 [setCookie] . fromStrict $ encodeUtf8 pageText

login :: Config.T -> Key -> Application
login c k req respond = case Session.get c k req of
  Nothing -> respond $ responseLBS status403 [] ""
  Just session -> do
    body <- strictRequestBody req
    -- TODO parse the body into params
    -- TODO check the token from the body against the session
    -- TODO check the domain of the email from the body
    -- TODO update the session with a new token for email verification
    -- TODO send the verification email
    -- TODO respond
    respond $ responseLBS status303 [(hLocation, "/")] ""

verify :: Config.T -> Key -> Application
verify = undefined

