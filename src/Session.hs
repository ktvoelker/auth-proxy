
module Session (loadKey, authenticated) where

import Data.ByteString (ByteString)
import Network.HTTP.Types
import Network.Wai
import Text.Email.Validate
import Web.ClientSession
import Web.Cookie

import qualified Config

loadKey :: Config.T -> IO Key
loadKey = getKey . Config.authKeyFile

parseRequestCookies :: Request -> [(ByteString, ByteString)]
parseRequestCookies =
  concatMap (parseCookies . snd)
  . filter ((== hCookie) . fst)
  . requestHeaders

authenticated :: Config.T -> Key -> Request -> Bool
authenticated c key req = sessionDomain == Just (Config.authEmailDomain c)
  where
    sessionDomain =
      lookup (Config.authCookie c) (parseRequestCookies req)
      >>= decrypt key
      >>= emailDomain

emailDomain :: ByteString -> Maybe ByteString
emailDomain = fmap domainPart . emailAddress

