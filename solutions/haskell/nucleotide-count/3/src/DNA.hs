module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map, empty, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = case nucleotides of
  Nothing -> Left xs
  Just n -> Right $ count n
 where
  count = foldr update empty
  update n = insertWith (+) n (1 :: Int)
  nucleotides =
    let
      readNucleotide char = case char of
        'A' -> Just A
        'G' -> Just G
        'T' -> Just T
        'C' -> Just C
        _ -> Nothing
     in
      traverse readNucleotide xs
