
module Session (T(..), loadKey, authenticated, get, new, setCookie) where

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
  (fmap domainPart $ get c key req >>= verifiedEmail)
  == Just (Config.authEmailDomain c)

get :: Config.T -> CS.Key -> Request -> Maybe T
get c key req =
  lookup (Config.authCookie c) (parseRequestCookies req)
  >>= CS.decrypt key
  >>= decode . fromStrict

setCookie :: Config.T -> CS.Key -> T -> IO Header
setCookie c k session = do
  bytes <- CS.encryptIO k . toStrict $ encode session
  return
    $ ( "Set-Cookie"
      , toStrict
        . toLazyByteString
        . renderSetCookie
        $ def
          { setCookieName = Config.authCookie c
          , setCookieValue = bytes
          , setCookieHttpOnly = True
          }
      )

new :: IO T
new = Session Nothing Nothing Nothing <$> getCurrentTime

