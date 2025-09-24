{-# OPTIONS_GHC -Wno-type-defaults #-}

module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

-- document: import Data.Function (on)
-- concat
-- intercalate
-- intersperse
-- unwords

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "negative size in group"

encode :: String -> String
encode xs = unwords $ transpose rows
  where
    norm = map toLower $ filter isAlphaNum xs
    nchars = fromIntegral $ length norm
    ncols = if lower ^ 2 == nchars then lower else upper
      where
        root = sqrt $ fromIntegral nchars
        lower = floor root
        upper = ceiling root
    nrows = if ncols * r >= nchars then r else ncols
      where
        r = ncols - 1
    string = norm ++ replicate (ncols * nrows - nchars) ' '
    rows = group ncols string
