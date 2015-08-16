
module Monad
  ( Error(..), ErrorType(..)
  , Uri, ContentType(..), Content(..), Success(..), SuccessType(..), RedirectType(..)
  , Env(), config, request, M(), makeApp
  ) where

import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid
import Data.Text.Encoding
import Network.HTTP.Types
import Network.Wai

import qualified Config
import Monad.Types
import qualified Session

plainText :: Header
plainText = (hContentType, "text/plain")

makeApp :: Config.T -> M Success -> Application
makeApp conf m req respond = do
  session <- maybe Session.new return (Session.get conf req)
  runReaderT (runExceptT (runStateT (unM m) session)) (Env conf req)
    >>= response conf session >>= respond

errorCode :: ErrorType -> Status
errorCode = \case
  BadRequest -> status400
  Forbidden  -> status403
  NotFound   -> status404
  Unknown    -> status500

successCode :: Success -> Status
successCode = \case
  NoContent      -> status204
  Success ty _ _ -> case ty of
    OK        -> status200
    Created _ -> status201
    Accepted  -> status202
  Redirect ty _  -> case ty of
    SeeOther  -> status303

contentTypeHeader :: ContentType -> Header
contentTypeHeader = (hContentType,) . \case
  ContentTypePlainText -> "text/plain"
  ContentTypeHTML -> "text/html"

successHeaders :: Success -> [Header]
successHeaders = \case
  NoContent       -> []
  Success ty ct _ -> [contentTypeHeader ct] <> case ty of
    OK          -> []
    Created loc -> [(hLocation, encodeUtf8 loc)]
    Accepted    -> []
  Redirect _ loc  -> [(hLocation, encodeUtf8 loc)]

contentLazyByteString :: Content -> LBS.ByteString
contentLazyByteString = \case
  TextContent xs           -> LBS.fromStrict $ encodeUtf8 xs
  ByteStringContent xs     -> LBS.fromStrict xs
  LazyByteStringContent xs -> xs

successBody :: Success -> LBS.ByteString
successBody = \case
  NoContent     -> ""
  Success _ _ c -> contentLazyByteString c
  Redirect _ _  -> ""

response :: Config.T -> Session.T -> Either Error (Success, Session.T) -> IO Response
response conf origSession = \case
  Left (Error ty msg) ->
    return . responseLBS (errorCode ty) [plainText] . LBS.fromStrict $ encodeUtf8 msg
  Right (success, session) -> successResponse success <$> sessionCookie session
  where
    sessionCookie session =
      if origSession == session
      then return []
      else (: []) <$> Session.setCookie conf session

successResponse :: Success -> [Header] -> Response
successResponse s hs =
  responseLBS (successCode s) (hs <> successHeaders s) (successBody s)

