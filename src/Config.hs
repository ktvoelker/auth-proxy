
{-# LANGUAGE TemplateHaskell #-}
module Config
  ( T(), serverUrl, serverPort, proxyHost, proxyPort, proxyKey
  , postmarkKey, postmarkSender, authCookie, authKey, authEmailDomain, authTitle, debug
  , load
  ) where

import Control.Lens
import Control.Monad
import Data.ByteString
import Data.Text
import Data.Text.Encoding
import qualified Web.ClientSession as CS

import qualified Config.File as F

data T = Config
  { _serverUrl       :: Text
  , _serverPort      :: Int
  , _proxyHost       :: ByteString
  , _proxyPort       :: Int
  , _proxyKey        :: CS.Key
  , _postmarkKey     :: ByteString
  , _postmarkSender  :: Text
  , _authCookie      :: ByteString
  , _authKey         :: CS.Key
  , _authEmailDomain :: ByteString
  , _authTitle       :: Text
  , _debug           :: Bool
  } deriving (Eq)

makeLenses ''T

load :: FilePath -> IO T
load = F.load >=> upgrade

upgrade :: F.T -> IO T
upgrade f =
  Config
  <$> pf F.serverUrl
  <*> pf F.serverPort
  <*> pb F.proxyHost
  <*> pf F.proxyPort
  <*> pk F.proxyKeyFile
  <*> pb F.postmarkKey
  <*> pf F.postmarkSender
  <*> pb F.authCookie
  <*> pk F.authKeyFile
  <*> pb F.authEmailDomain
  <*> pf F.authTitle
  <*> pf F.debug
  where
    pf :: (F.T -> a) -> IO a
    pf = pure . ($ f)
    pb :: (F.T -> Text) -> IO ByteString
    pb = (encodeUtf8 <$>) . pf
    pk :: (F.T -> FilePath) -> IO CS.Key
    pk = CS.getKey . ($ f)

