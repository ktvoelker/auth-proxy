
{-# LANGUAGE TemplateHaskell #-}
module Monad.Types where

import Control.Lens
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Network.Wai

import qualified Config
import qualified Session

data Error = Error ErrorType T.Text
  deriving (Eq, Ord, Show)

data ErrorType = BadRequest | Forbidden | NotFound | Unknown
  deriving (Eq, Ord, Show)

type Uri = T.Text

data Success =
    NoContent
  | Success SuccessType T.Text
  | Redirect RedirectType Uri
  deriving (Eq, Ord, Show)

data SuccessType = OK | Created Uri | Accepted
  deriving (Eq, Ord, Show)

data RedirectType = SeeOther
  deriving (Eq, Ord, Show)

data Env =
  Env
  { _config  :: Config.T
  , _request :: Request
  }

makeLenses ''Env

newtype M a = M { unM :: StateT Session.T (ExceptT Error (ReaderT Env IO)) a }
  deriving
    ( Functor, Applicative, Monad
    , MonadState Session.T, MonadError Error, MonadReader Env, MonadIO
    )

