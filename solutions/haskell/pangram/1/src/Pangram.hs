module Pangram (isPangram) where

import Data.Char (isAlpha, isAscii, toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text = 26 == length (nub $ filter isAsciiAlpha $ map toLower text)
 where
  isAsciiAlpha x = isAscii x && isAlpha x
