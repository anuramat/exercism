module Anagram (anagramsFor) where
import Data.List (sort)
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter $ equiv xs where
  l = map toLower
  equiv x y = sort (l x) == sort (l y) && l x /= l y

