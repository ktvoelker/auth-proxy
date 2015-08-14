
module Config (T, load) where

data T = Config { }
  deriving (Eq, Ord, Show)

load :: FilePath -> IO (Maybe T)
load = const $ return $ Just Config

