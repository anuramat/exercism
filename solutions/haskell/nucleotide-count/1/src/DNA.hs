module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map, empty, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = case count nucleotides of
  Nothing -> Left xs
  Just m -> Right m
 where
  count = foldr update (Just empty)
  nucleotides :: [Maybe Nucleotide]
  nucleotides = map readNucleotide xs
  update :: Maybe Nucleotide -> Maybe (Map Nucleotide Int) -> Maybe (Map Nucleotide Int)
  update (Just n) (Just m) = Just $ insertWith (+) n (1 :: Int) m
  update _ _ = Nothing

readNucleotide :: Char -> Maybe Nucleotide
readNucleotide char = case char of
  'A' -> Just A
  'G' -> Just G
  'T' -> Just T
  'C' -> Just C
  _ -> Nothing
