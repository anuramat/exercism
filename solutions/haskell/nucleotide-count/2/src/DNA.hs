module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map, empty, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = case nucleotides of
  Nothing -> Left xs
  Just nuc -> Right $ count nuc
 where
  count = foldr update empty
  nucleotides :: Maybe [Nucleotide]
  nucleotides = traverse readNucleotide xs
  update :: Nucleotide -> Map Nucleotide Int -> Map Nucleotide Int
  update n = insertWith (+) n (1 :: Int)

readNucleotide :: Char -> Maybe Nucleotide
readNucleotide char = case char of
  'A' -> Just A
  'G' -> Just G
  'T' -> Just T
  'C' -> Just C
  _ -> Nothing
