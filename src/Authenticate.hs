
module Authenticate (app) where

import Network.HTTP.Types
import Network.Wai
import Web.ClientSession (Key)

import qualified Config
import HTML

app :: Config.T -> Key -> Application
app _ _ _ respond = respond $ responseLBS status403 [] ""

