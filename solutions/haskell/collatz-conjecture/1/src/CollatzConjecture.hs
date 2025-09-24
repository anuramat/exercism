module CollatzConjecture (collatz) where

import Data.List

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = fmap toInteger index
 where
  index = elemIndex 1 $ iterate nextNumber n
  nextNumber x = if even x then div x 2 else 3 * x + 1
