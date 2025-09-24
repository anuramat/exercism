module Bob (responseFor) where

import Data.Char (isAlpha, isLower, isSpace)

responseFor :: String -> String
responseFor raw
  | yell && question = "Calm down, I know what I'm doing!"
  | question = "Sure."
  | yell = "Whoa, chill out!"
  | silence = "Fine. Be that way!"
  | otherwise = "Whatever."
 where
  text = reverse $ dropWhile isSpace (reverse raw)
  question = not (null text) && last text == '?'
  yell = any isAlpha text && not (any isLower text)
  silence = null text
