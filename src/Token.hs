
module Token (new) where

import qualified Data.Text as T
import System.Random (randomRs)
import System.Random.TF (newTFGen)

tokenLength :: Int
tokenLength = 128

new :: IO T.Text
new = T.pack . take tokenLength . randomRs ('A', 'Z') <$> newTFGen

