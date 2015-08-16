
module Proxy (newApp) where

import Control.Lens
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.ReverseProxy
import Network.Wai
import Text.Email.Validate

import qualified Claim
import qualified Config
import qualified Session
import qualified Validate

proxyDest :: Config.T -> ProxyDest
proxyDest c = ProxyDest (view Config.proxyHost c) (view Config.proxyPort c)

newApp :: Config.T -> IO Application
newApp conf =
  newManager defaultManagerSettings
  >>= return . waiProxyTo proxyResp defaultOnExc
  where
    dest = proxyDest conf
    noClaim = return $ WPRProxyDest dest
    proxyResp req = case Session.get conf req of
      Nothing -> noClaim
      Just s -> case view Session.verifiedEmail s of
        Nothing -> noClaim
        Just email ->
          if Validate.emailOK conf email
          then flip WPRModifiedRequest dest <$> addClaim conf email req
          else noClaim

addClaim :: Config.T -> EmailAddress -> Request -> IO Request
addClaim conf email req = do
  claim <- Claim.assert conf email
  let claimHeader = (Claim.headerName, claim)
  return $ req { requestHeaders = claimHeader : requestHeaders req }

