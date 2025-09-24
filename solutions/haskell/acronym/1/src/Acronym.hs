-- {-# LANGUAGE OverloadedStrings #-}

module Acronym (abbreviate) where

import Data.Char (isSpace, isUpper, toUpper)
import qualified Data.Text as T

abbreviate :: T.Text -> T.Text
abbreviate xs =
  T.pack firstChars
 where
  firstChars = partsWithoutAbbreviations >>= splitWords . T.unpack
   where
    partsWithoutAbbreviations = map (\x -> if all isUpper $ T.unpack x then T.take 1 x else x) parts
     where
      parts = filter (not . T.null) $ T.split isSpace clean
       where
        clean = T.pack $ map hehe (T.unpack xs)
        hehe :: Char -> Char
        hehe x
          | x `elem` "-_" = ' '
          | otherwise = x

splitWords :: String -> String
splitWords s = go True s []
 where
  go firstChar (x : xs) acc
    | firstChar || isUpper x = go False xs $ acc ++ [toUpper x]
    | otherwise = go False xs acc
  go _ "" acc = acc
