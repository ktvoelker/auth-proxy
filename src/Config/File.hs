
module Config.File (T(..), load) where

import Control.Monad
import Data.Text
import Data.Yaml

data T = ConfigFile
  { serverUrl          :: Text
  , serverPort         :: Int
  , serverAllowOrigins :: [Text]
  , proxyHost          :: Text
  , proxyPort          :: Int
  , proxyKeyFile       :: FilePath
  , postmarkKey        :: Text
  , postmarkSender     :: Text
  , authCookie         :: Text
  , authKeyFile        :: FilePath
  , authEmailDomain    :: Text
  , authTitle          :: Text
  , debug              :: Bool
  } deriving (Eq, Ord)

(..:) :: (FromJSON a) => Object -> (Text, Text) -> Parser a
(..:) obj (j, k) = obj .: j >>= (.: k)

instance FromJSON T where
  parseJSON (Object v) =
    ConfigFile
      <$> v ..: ("server", "url")
      <*> v ..: ("server", "port")
      <*> v ..: ("server", "allow-origins")
      <*> v ..: ("proxy", "host")
      <*> v ..: ("proxy", "port")
      <*> v ..: ("proxy", "key-file")
      <*> v ..: ("postmark", "key")
      <*> v ..: ("postmark", "sender")
      <*> v ..: ("authentication", "cookie")
      <*> v ..: ("authentication", "key-file")
      <*> v ..: ("authentication", "email-domain")
      <*> v ..: ("authentication", "title")
      <*> v .:? "debug" .!= False
  parseJSON _ = mzero

load :: FilePath -> IO T
load = decodeFileEither >=> either (error . show) return

