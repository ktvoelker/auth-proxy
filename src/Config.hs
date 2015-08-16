
module Config (T(..), load) where

import Control.Monad
import Data.ByteString
import Data.Text
import Data.Text.Encoding
import qualified Web.ClientSession as CS

import qualified Config.File as F

data T = Config
  { serverUrl       :: Text
  , serverPort      :: Int
  , proxyHost       :: ByteString
  , proxyPort       :: Int
  , proxyKey        :: CS.Key
  , postmarkKey     :: ByteString
  , postmarkSender  :: Text
  , authCookie      :: ByteString
  , authKey         :: CS.Key
  , authEmailDomain :: ByteString
  , authTitle       :: Text
  } deriving (Eq)

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
  where
    pf :: (F.T -> a) -> IO a
    pf = pure . ($ f)
    pb :: (F.T -> Text) -> IO ByteString
    pb = (encodeUtf8 <$>) . pf
    pk :: (F.T -> FilePath) -> IO CS.Key
    pk = CS.getKey . ($ f)

