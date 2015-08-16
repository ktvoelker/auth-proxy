
module Claim (C.headerName, assert) where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Network.HTTP.AuthProxy.Claim as C
import Text.Email.Validate (EmailAddress)

import qualified Config

assert :: Config.T -> EmailAddress -> IO ByteString
assert = C.assert . view Config.proxyKey

