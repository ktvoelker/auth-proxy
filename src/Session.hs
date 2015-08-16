
module Session
  ( T(), new, created, token, unverifiedEmail, verifiedEmail
  , authenticated, get, setCookie, emailOK
  ) where

import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Network.HTTP.Types
import Network.Wai
import Text.Email.Validate
import qualified Web.ClientSession as CS
import Web.Cookie

import qualified Config
import Session.Types

parseRequestCookies :: Request -> [(ByteString, ByteString)]
parseRequestCookies =
  concatMap (parseCookies . snd)
  . filter ((== hCookie) . fst)
  . requestHeaders

emailOK :: Config.T -> EmailAddress -> Bool
emailOK c = (== Config.authEmailDomain c) . domainPart

authenticated :: Config.T -> Request -> Bool
authenticated c req = maybe False (emailOK c) $ get c req >>= view verifiedEmail

get :: Config.T -> Request -> Maybe T
get c req =
  lookup (Config.authCookie c) (parseRequestCookies req)
  >>= CS.decrypt (Config.authKey c)
  >>= decode . fromStrict

setCookie :: Config.T -> T -> IO Header
setCookie c session = do
  bytes <- CS.encryptIO (Config.authKey c) . toStrict $ encode session
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

