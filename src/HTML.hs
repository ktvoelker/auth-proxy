
module HTML where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

import Paths_auth_proxy

login :: IO FilePath
login = getDataFileName "login.html"

render :: [(Text, Text)] -> FilePath -> IO Text
render args fp = renderText args <$> readFile fp

renderText :: [(Text, Text)] -> Text -> Text
renderText args xs = foldl f xs args
  where
    f xs (key, val) = T.replace key val xs

