module CollatzConjecture (collatz) where

import Data.List (unfoldr)

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = Just $ toInteger $ length $ unfoldr nextNumber n
 where
  nextNumber 1 = Nothing
  nextNumber x = Just (x, if even x then div x 2 else 3 * x + 1)
