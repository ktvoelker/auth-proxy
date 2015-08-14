
module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Config
import HTML

main :: IO ()
main = getArgs >>= \case
  [configPath] -> do
    config <- Config.load configPath
    loginTemplatePath >>= putStrLn
  _ -> do
    hPutStrLn stderr "Usage: auth-proxy CONFIG_FILE"
    exitFailure

