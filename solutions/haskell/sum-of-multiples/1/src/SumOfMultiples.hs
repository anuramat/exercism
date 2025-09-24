module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum multiples
 where
  multiples = nub $ concatMap factorMultiples factors
   where
    factorMultiples 0 = [0]
    factorMultiples x = takeWhile (< limit) [i * x | i <- [1 ..]]
