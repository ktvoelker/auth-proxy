
module Authenticate (app) where

import Control.Lens
import Control.Monad
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Monoid
import Data.Text.Encoding
import Network.HTTP.Types
import Network.Wai
import Network.Waitra
import Text.Email.Validate

import qualified Config
import qualified Email
import qualified HTML
import Monad
import qualified Session
import qualified Token

plainText :: Header
plainText = (hContentType, "text/plain")

app :: Config.T -> Application
app c = waitraMiddleware routes redirectToLogin
  where
    routes =
      [ simpleGet "/login" $ loginPage c
      , simplePost "/login" $ login c
      , simpleGet "/verify" $ verify c
      ]

redirectToLogin :: Application
redirectToLogin = const ($ responseLBS status303 [(hLocation, "/login")] "")

loginPage :: Config.T -> Application
loginPage c req respond = do
  session <- maybe Session.new return $ Session.get c req
  (token, session') <- case view Session.token session of
    Just t -> return (t, session)
    Nothing -> do
      t <- Token.new
      let session' = set Session.token (Just t) session
      return (t, session')
  setCookie <-
    if session == session'
    then return []
    else (: []) <$> Session.setCookie c session'
  let
    args =
      [ ("__TITLE__", view Config.authTitle c)
      , ("__TOKEN__", token)
      ]
  pageText <- HTML.login >>= HTML.render args
  respond . responseLBS status200 setCookie . fromStrict $ encodeUtf8 pageText

login :: Config.T -> Application
login c req respond = case Session.get c req of
  Nothing -> respond $ responseLBS status403 [] ""
  Just session -> do
    args <- parseQuery . toStrict <$> strictRequestBody req
    case (decodeUtf8 <$> join (lookup "token" args), view Session.token session) of
      (Just actual, Just expected) | actual == expected -> do
        case join (lookup "email" args) >>= emailAddress of
          Just email | Session.emailOK c email -> do
            emailToken <- Token.new
            let
              session' =
                set Session.unverifiedEmail (Just (emailToken, email)) session
            setCookie <- Session.setCookie c session'
            emailStatus <- Email.send c
              $ Email.Email
                { Email.to = decodeUtf8 $ toByteString email
                , Email.from = view Config.postmarkSender c
                , Email.subject = "Your login link for " <> view Config.authTitle c
                , Email.textBody
                  = view Config.serverUrl c <> "/verify?token=" <> emailToken
                }
            case emailStatus of
              200 -> respond
                $ responseLBS
                  status200
                  [setCookie, plainText]
                  "Check your email!"
              _ -> respond $ responseLBS status500 [plainText] "Failed to send email."
          _ -> respond $ responseLBS status400 [plainText] "Invalid domain."
      _ -> respond $ responseLBS status400 [plainText] "Invalid token."

verify :: Config.T -> Application
verify c req respond = case Session.get c req of
  Nothing -> respond $ responseLBS status403 [] ""
  Just session -> do
    let args = queryString req
    let mActualToken = decodeUtf8 <$> join (lookup "token" args)
    case (mActualToken, view Session.unverifiedEmail session) of
      (Just actualToken, Just (expectedToken, unverifiedEmail))
        | actualToken == expectedToken && Session.emailOK c unverifiedEmail -> do
          let
            session' =
              set Session.verifiedEmail (Just unverifiedEmail)
              . set Session.unverifiedEmail Nothing
              $ session
          setCookie <- Session.setCookie c session'
          respond $ responseLBS status303 [setCookie, (hLocation, "/")] ""
      _ -> respond $ responseLBS status403 [] ""

