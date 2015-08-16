
module Authenticate (routes) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
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

routes :: Config.T -> [Route]
routes conf =
  [ simpleGet  "/auth/check"  $ makeApp conf check
  , simpleGet  "/auth/login"  $ makeApp conf loginPage
  , simplePost "/auth/login"  $ makeApp conf login
  , simpleGet  "/auth/logout" $ makeApp conf logout
  , simpleGet  "/auth/verify" $ makeApp conf verify
  ]

check :: M Success
check = use Session.verifiedEmail >>= \case
  Nothing -> throwError $ Error Forbidden "Not authenticated."
  Just email ->
    return . Success OK ContentTypePlainText . ByteStringContent $ toByteString email

logout :: M Success
logout = use Session.verifiedEmail >>= \case
  Nothing -> throwError $ Error Forbidden "Not authenticated."
  Just _ -> liftIO Session.new >>= put >> return (Redirect SeeOther "/auth/login")

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
  return . Success OK ContentTypeHTML $ TextContent pageText

checkEmail :: EmailAddress -> ErrorType -> M ()
checkEmail email et =
  fmap not (emailOK email)
  >>= flip when (throwError $ Error et "Invalid email domain.")

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
  checkEmail email BadRequest
  emailToken <- liftIO Token.new
  assign Session.unverifiedEmail $ Just (emailToken, email)
  conf <- view config
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
      , Email.textBody = serverUrl <> "/auth/verify?token=" <> emailToken
      }
  when (emailStatus /= 200)
    $ throwError $ Error Unknown "Failed to send email."
  return . Success OK ContentTypePlainText $ LazyByteStringContent "Check your email!"

verify :: M Success
verify = do
  args <- view $ request . to queryString
  actualToken <- case decodeUtf8 <$> join (lookup "token" args) of
    Nothing -> throwError $ Error BadRequest "Missing token."
    Just token -> return token
  (expectedToken, unverifiedEmail) <- use Session.unverifiedEmail >>= \case
    Nothing -> throwError $ Error Forbidden "Invalid session."
    Just p -> return p
  checkEmail unverifiedEmail Forbidden
  when (actualToken /= expectedToken)
    $ throwError $ Error Forbidden "Invalid session."
  assign Session.unverifiedEmail Nothing
  assign Session.verifiedEmail $ Just unverifiedEmail
  return $ Redirect SeeOther "/"

