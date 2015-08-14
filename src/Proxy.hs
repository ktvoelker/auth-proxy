
module Proxy (app) where

import Network.HTTP.Client
import Network.HTTP.ReverseProxy
import Network.Wai

import qualified Config

proxyDest :: Config.T -> ProxyDest
proxyDest c = ProxyDest (Config.proxyHost c) (Config.proxyPort c)

app :: Config.T -> Application
app c req respond = do
  mgr <- newManager defaultManagerSettings
  waiProxyTo
    (const . return . WPRProxyDest $ proxyDest c)
    defaultOnExc
    mgr
    req
    respond

