module Bob (responseFor) where

import Data.Char (isAlpha, isUpper, isSpace)

responseFor :: String -> String
responseFor raw
  | yell && question = "Calm down, I know what I'm doing!"
  | question = "Sure."
  | yell = "Whoa, chill out!"
  | silence = "Fine. Be that way!"
  | otherwise = "Whatever."
 where
  text = reverse $ dropWhile isSpace $ reverse raw
  question = not (null text) && last text == '?'
  yell = let alphas = filter isAlpha text in not (null alphas) && all isUpper alphas
  silence = null text
