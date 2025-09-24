module CollatzConjecture (collatz) where

import Data.List ()

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = Just $ toInteger $ length $ takeWhile (/= 1) $ iterate nextNumber n
 where
  nextNumber x = if even x then div x 2 else 3 * x + 1
