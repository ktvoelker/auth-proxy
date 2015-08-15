
module Config (T(..), load) where

import Control.Monad
import Data.ByteString
import Data.Text
import Data.Text.Encoding
import Data.Yaml

data T = Config
  { serverPort      :: Int
  , proxyHost       :: ByteString
  , proxyPort       :: Int
  , proxyKeyFile    :: FilePath
  , postmarkKey     :: Text
  , postmarkSender  :: Text
  , authCookie      :: ByteString
  , authKeyFile     :: FilePath
  , authEmailDomain :: ByteString
  , authTitle       :: Text
  } deriving (Eq, Ord)

(..:) :: (FromJSON a) => Object -> (Text, Text) -> Parser a
(..:) obj (j, k) = obj .: j >>= (.: k)

instance FromJSON T where
  parseJSON (Object v) =
    Config
      <$> v ..: ("server", "port")
      <*> fmap encodeUtf8 (v ..: ("proxy", "host"))
      <*> v ..: ("proxy", "port")
      <*> v ..: ("proxy", "key-file")
      <*> v ..: ("postmark", "key")
      <*> v ..: ("postmark", "sender")
      <*> fmap encodeUtf8 (v ..: ("authentication", "cookie"))
      <*> v ..: ("authentication", "key-file")
      <*> fmap encodeUtf8 (v ..: ("authentication", "email-domain"))
      <*> v ..: ("authentication", "title")

load :: FilePath -> IO T
load = decodeFileEither >=> either (error . show) return

