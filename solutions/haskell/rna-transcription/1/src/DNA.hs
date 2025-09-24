module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse translateOne

translateOne :: Char -> Either Char Char
translateOne x = case x of
  'G' -> Right 'C'
  'C' -> Right 'G'
  'T' -> Right 'A'
  'A' -> Right 'U'
  y -> Left y
