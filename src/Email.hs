
module Email (T(..), send) where

import Control.Lens (view, set)
import Data.Aeson
import Data.Text (Text)
import Network.Wreq

import qualified Config

data T =
  Email
  { to       :: Text
  , from     :: Text
  , subject  :: Text
  , textBody :: Text
  } deriving (Eq, Ord, Show)

instance ToJSON T where
  toJSON Email{..} =
    object
      [ "To" .= to
      , "From" .= from
      , "Subject" .= subject
      , "TextBody" .= textBody
      , "TrackOpens" .= False
      ]

opts :: Config.T -> Options
opts c =
  set (header "X-Postmark-Server-Token") [view Config.postmarkKey c]
  . set (header "Accept") ["application/json"]
  . set (header "Content-Type") ["application/json"]
  . set checkStatus Nothing
  $ defaults

postmarkUri :: [Char]
postmarkUri = "https://api.postmarkapp.com/email"

send :: Config.T -> T -> IO Int
send c email =
  view (responseStatus . statusCode) <$> postWith (opts c) postmarkUri (encode email)

