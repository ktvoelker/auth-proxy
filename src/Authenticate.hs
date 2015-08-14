
module Authenticate (app) where

import Network.HTTP.Types
import Network.Wai

import qualified Config

app :: Config.T -> Application
app _ _ respond = respond $ responseLBS status403 [] ""

