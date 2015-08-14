
module Config (T(..), load) where

import Control.Monad
import Data.Text
import Data.Yaml

data T = Config
  { proxyHost       :: Text
  , proxyPort       :: Int
  , postmarkKey     :: Text
  , postmarkSender  :: Text
  , authSecret      :: Text
  , authEmailDomain :: Text
  , authTitle       :: Text
  } deriving (Eq, Ord)

(..:) :: (FromJSON a) => Object -> (Text, Text) -> Parser a
(..:) obj (j, k) = obj .: j >>= (.: k)

instance FromJSON T where
  parseJSON (Object v) =
    Config
      <$> v ..: ("proxy", "host")
      <*> v ..: ("proxy", "port")
      <*> v ..: ("postmark", "key")
      <*> v ..: ("postmark", "sender")
      <*> v ..: ("authentication", "secret")
      <*> v ..: ("authentication", "email-domain")
      <*> v ..: ("authentication", "title")

load :: FilePath -> IO T
load = decodeFileEither >=> either (error . show) return

