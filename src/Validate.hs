
module Validate (emailOK) where

import Control.Lens
import Text.Email.Validate

import qualified Config

emailOK :: Config.T -> EmailAddress -> Bool
emailOK conf actualEmail = domainPart actualEmail == view Config.authEmailDomain conf

