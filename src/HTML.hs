
module HTML where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

import Paths_auth_proxy

loginTemplatePath :: IO FilePath
loginTemplatePath = getDataFileName "login.html"

fillTemplate :: [(Text, Text)] -> FilePath -> IO Text
fillTemplate args fp = fillTemplateText args <$> readFile fp

fillTemplateText :: [(Text, Text)] -> Text -> Text
fillTemplateText args xs = foldl f xs args
  where
    f xs (key, val) = T.replace key val xs
