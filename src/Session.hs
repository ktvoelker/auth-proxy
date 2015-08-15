
module Session (T(..), loadKey, authenticated, get, new) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import Network.HTTP.Types
import Network.Wai
import System.Random
import System.Random.TF
import Text.Email.Validate
import qualified Web.ClientSession as CS
import Web.Cookie

import qualified Config
import Session.Types

loadKey :: Config.T -> IO CS.Key
loadKey = CS.getKey . Config.authKeyFile

parseRequestCookies :: Request -> [(ByteString, ByteString)]
parseRequestCookies =
  concatMap (parseCookies . snd)
  . filter ((== hCookie) . fst)
  . requestHeaders

authenticated :: Config.T -> CS.Key -> Request -> Bool
authenticated c key req =
  (fmap domainPart $ get c key req >>= email) == Just (Config.authEmailDomain c)

get :: Config.T -> CS.Key -> Request -> Maybe T
get c key req =
  lookup (Config.authCookie c) (parseRequestCookies req)
  >>= CS.decrypt key
  >>= decode . fromStrict

tokenLength :: Int
tokenLength = 128

newToken :: IO T.Text
newToken = T.pack . take tokenLength . randomRs ('A', 'Z') <$> newStdGen

setCookie :: ByteString -> ByteString -> Header
setCookie n v =
  ( "Set-Cookie"
  , toStrict
    . toLazyByteString
    . renderSetCookie
    $ def { setCookieName = n, setCookieValue = v, setCookieHttpOnly = True }
  )

new :: Config.T -> CS.Key -> IO (T, [Header])
new c k = do
  session <- Session <$> pure Nothing <*> getCurrentTime <*> newToken
  cookieBytes <- CS.encryptIO k . toStrict $ encode session
  return (session, [setCookie (Config.authCookie c) cookieBytes])

