
module Authenticate (app) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Types
import Network.Wai
import Network.Waitra
import Text.Email.Validate
import Web.ClientSession (Key)

import qualified Config
import qualified HTML
import qualified Session
import qualified Token

plainText :: Header
plainText = (hContentType, "text/plain")

app :: Config.T -> Key -> Application
app c k = waitraMiddleware routes redirectToLogin
  where
    routes =
      [ simpleGet "/login" $ loginPage c k
      , simplePost "/login" $ login c k
      , simpleGet "/verify" $ verify c k
      ]

redirectToLogin :: Application
redirectToLogin = const ($ responseLBS status303 [(hLocation, "/login")] "")

loginPage :: Config.T -> Key -> Application
loginPage c k req respond = do
  session <- maybe Session.new return $ Session.get c k req
  (token, session') <- case Session.token session of
    Just t -> return (t, session)
    Nothing -> do
      t <- Token.new
      let session' = session { Session.token = Just t }
      return (t, session')
  setCookie <-
    if session == session'
    then return []
    else (: []) <$> Session.setCookie c k session'
  let
    args =
      [ ("__TITLE__", Config.authTitle c)
      , ("__TOKEN__", token)
      ]
  pageText <- HTML.login >>= HTML.render args
  respond . responseLBS status200 setCookie . fromStrict $ encodeUtf8 pageText

login :: Config.T -> Key -> Application
login c k req respond = case Session.get c k req of
  Nothing -> respond $ responseLBS status403 [] ""
  Just session -> do
    args <- parseQuery . toStrict <$> strictRequestBody req
    case (decodeUtf8 <$> join (lookup "token" args), Session.token session) of
      (Just actual, Just expected) | actual == expected -> do
        case join (lookup "email" args) >>= emailAddress of
          Just email | Session.emailOK c email -> do
            emailToken <- Token.new
            let
              session' =
                session
                { Session.unverifiedEmail = Just (emailToken, email)
                }
            setCookie <- Session.setCookie c k session'
            let
              verifyUri =
                Config.serverUrl c <> "/verify?token=" <> encodeUtf8 emailToken
            print verifyUri
            -- TODO send the verification email
            respond
              $ responseLBS
                status200
                [setCookie, plainText]
                "Check your email!"
          _ -> respond $ responseLBS status400 [plainText] "Invalid domain."
      _ -> respond $ responseLBS status400 [plainText] "Invalid token."

verify :: Config.T -> Key -> Application
verify c k req respond = case Session.get c k req of
  Nothing -> respond $ responseLBS status403 [] ""
  Just session -> do
    let args = queryString req
    case (decodeUtf8 <$> join (lookup "token" args), Session.unverifiedEmail session) of
      (Just actualToken, Just (expectedToken, unverifiedEmail))
        | actualToken == expectedToken && Session.emailOK c unverifiedEmail -> do
          let
            session' = session
              { Session.verifiedEmail = Just unverifiedEmail
              , Session.unverifiedEmail = Nothing
              }
          setCookie <- Session.setCookie c k session'
          respond $ responseLBS status303 [setCookie, (hLocation, "/")] ""
      _ -> respond $ responseLBS status403 [] ""

