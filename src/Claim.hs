
module Claim (assert, check) where

import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Text.Email.Validate
import qualified Web.ClientSession as CS

import qualified Config

assert :: Config.T -> EmailAddress -> IO BS.ByteString
assert conf = CS.encryptIO (view Config.proxyKey conf) . toByteString

check :: Config.T -> BS.ByteString -> Maybe EmailAddress
check conf = CS.decrypt (view Config.proxyKey conf) >=> emailAddress

