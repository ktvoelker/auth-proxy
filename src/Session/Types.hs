
{-# LANGUAGE TemplateHaskell #-}
module Session.Types where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time.Clock
import qualified Data.Vector as V
import Text.Email.Validate

instance ToJSON EmailAddress where
  toJSON = toJSON . BS.unpack . toByteString

instance FromJSON EmailAddress where
  parseJSON (Array xs) = do
    byteList <- mapM parseJSON $ V.toList xs
    case emailAddress $ BS.pack byteList of
      Nothing -> mzero
      Just e -> return e
  parseJSON _ = mzero

data T =
  Session
  { token           :: Maybe T.Text
  , verifiedEmail   :: Maybe EmailAddress
  , unverifiedEmail :: Maybe (T.Text, EmailAddress)
  , created         :: UTCTime
  } deriving (Eq, Ord)

deriveJSON defaultOptions ''T

