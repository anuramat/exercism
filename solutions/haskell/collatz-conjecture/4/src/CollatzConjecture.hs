module CollatzConjecture (collatz) where

import Data.List (unfoldr)

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | n == 1 = Just 0
  | otherwise = (1 +) <$> collatz (if even n then div n 2 else 3 * n + 1)
