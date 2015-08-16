
module Network.HTTP.AuthProxy.Claim
  ( CS.getKey, CS.Key
  , headerName, assert, check, Options(..), defaultOptions, checkMiddleware
  ) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Maybe
import Network.HTTP.Types
import Network.Wai
import Text.Email.Validate
import qualified Web.ClientSession as CS

headerName :: HeaderName
headerName = "X-Authenticated-Email"

assert :: CS.Key -> EmailAddress -> IO BS.ByteString
assert key = CS.encryptIO key . toByteString

check :: CS.Key -> BS.ByteString -> Maybe EmailAddress
check key = CS.decrypt key >=> emailAddress

data Options =
  Options
  { requireAuthentication :: Bool
  , requireEmailDomain :: Maybe BS.ByteString
  } deriving (Eq, Ord, Show)

defaultOptions :: Options
defaultOptions =
  Options
  { requireAuthentication = True
  , requireEmailDomain = Nothing
  }

type ListElemView a = (Maybe a, ([a] -> [a], [a]))

findView :: (a -> Bool) -> [a] -> ListElemView a
findView p = f id
  where
    f prevs = \case
      [] -> (Nothing, (prevs, []))
      (x : xs)
        | p x -> (Just x, (prevs, xs))
        | otherwise -> f (prevs . (x :)) xs

setElem :: Maybe a -> ListElemView a -> ListElemView a
setElem x (_, ctx) = (x, ctx)

getElem :: ListElemView a -> Maybe a
getElem = fst

reassemble :: ListElemView a -> [a]
reassemble (x, (prevs, xs)) = prevs $ maybeToList x ++ xs

checkMiddleware :: CS.Key -> Options -> Middleware
checkMiddleware key Options{..} app req respond =
  if allow
  then app req' respond
  else respond $ responseLBS status303 [(hLocation, "/auth/login")] ""
  where
    encHeaderView = findView ((== headerName) . fst) $ requestHeaders req
    encHeader = fmap snd $ getElem encHeaderView
    claimEmail = encHeader >>= check key
    acceptedClaim = case (requireEmailDomain, claimEmail) of
      (Nothing, _) -> claimEmail
      (Just expectDomain, Just actualEmail)
        | domainPart actualEmail == expectDomain -> claimEmail
      _ -> Nothing
    decHeader = fmap ((headerName,) . toByteString) acceptedClaim
    headers' = reassemble $ setElem decHeader encHeaderView
    req' = req { requestHeaders = headers' }
    allow = not requireAuthentication || isJust acceptedClaim

