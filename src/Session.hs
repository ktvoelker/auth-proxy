
module Session where

import Network.Wai

import qualified Config

authenticated :: Config.T -> Request -> Bool
authenticated _ _ = False

