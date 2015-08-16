
module Authenticate (app) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.ByteString.Lazy (toStrict)
import Data.Maybe
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

app :: Config.T -> Application
app conf = waitraMiddleware routes (makeApp conf redirectToLogin)
  where
    routes =
      [ simpleGet  "/login"  $ makeApp conf loginPage
      , simplePost "/login"  $ makeApp conf login
      , simpleGet  "/verify" $ makeApp conf verify
      ]

redirectToLogin :: M Success
redirectToLogin = return $ Redirect SeeOther "/login"

loginPage :: M Success
loginPage = do
  token <- use Session.token >>= \case
    Just token -> return token
    Nothing -> do
      token <- liftIO Token.new
      assign Session.token $ Just token
      return token
  title <- view $ config . Config.authTitle
  let args = [("__TITLE__", title), ("__TOKEN__", token)]
  pageText <- liftIO $ HTML.login >>= HTML.render args
  return $ Success OK pageText

login :: M Success
login = do
  -- TODO this should probably be a "framework" feature
  args <- fmap (parseQuery . toStrict) $ view request >>= liftIO . strictRequestBody
  let actualToken = decodeUtf8 <$> join (lookup "token" args)
  when (isNothing actualToken)
    $ throwError $ Error Forbidden "Missing token."
  expectedToken <- use Session.token
  when (actualToken /= expectedToken)
    $ throwError $ Error Forbidden "Invalid token."
  email <- case join (lookup "email" args) >>= emailAddress of
    Nothing -> throwError $ Error BadRequest "Missing email."
    Just email -> return email
  conf <- view config
  -- TODO move emailOK into the monad M (and does it belong in Session?)
  when (not $ Session.emailOK conf email)
    $ throwError $ Error BadRequest "Invalid email domain."
  emailToken <- liftIO Token.new
  assign Session.unverifiedEmail $ Just (emailToken, email)
  sender <- view $ config . Config.postmarkSender
  authTitle <- view $ config . Config.authTitle
  serverUrl <- view $ config . Config.serverUrl
  -- TODO move Email.send into the monad M
  emailStatus <-
    liftIO . Email.send conf
    $ Email.Email
      { Email.to = decodeUtf8 $ toByteString email
      , Email.from = sender
      , Email.subject = "Your login link for " <> authTitle
      , Email.textBody = serverUrl <> "/verify?token=" <> emailToken
      }
  when (emailStatus /= 200)
    $ throwError $ Error Unknown "Failed to send email."
  return $ Success OK "Check your email!"

verify :: M Success
verify = do
  args <- view $ request . to queryString
  actualToken <- case decodeUtf8 <$> join (lookup "token" args) of
    Nothing -> throwError $ Error BadRequest "Missing token."
    Just token -> return token
  (expectedToken, unverifiedEmail) <- use Session.unverifiedEmail >>= \case
    Nothing -> throwError $ Error Forbidden "Invalid session."
    Just p -> return p
  conf <- view config
  when (not $ Session.emailOK conf unverifiedEmail)
    $ throwError $ Error Forbidden "Invalid email."
  when (actualToken /= expectedToken)
    $ throwError $ Error Forbidden "Invalid session."
  assign Session.unverifiedEmail Nothing
  assign Session.verifiedEmail $ Just unverifiedEmail
  return $ Redirect SeeOther "/"

