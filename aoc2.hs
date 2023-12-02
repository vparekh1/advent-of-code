import Data.Char (toLower)
import Data.Map
import Data.Monoid
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

-- Main function
main :: IO ()
main =
  let elfBag = Bag {red = 12, green = 13, blue = 14}
   in do
        bagGames <- parseFromFile aocFile "aoc2.txt"
        case bagGames of
          Left err -> print err
          Right bag -> do
            putStrLn "First aoc2 problem: "
            print $ sum $ fst <$> Prelude.filter (not . isBiggerThanAny elfBag) bag
            putStrLn "Second aoc2 problem:"
            print $ sum $ bagPower . minBag <$> bag

-- Utilities
bagPower :: Bag -> Int
bagPower b = red b * blue b * green b

minBag :: (Int, [Bag]) -> Bag
minBag parsedBag = mconcat $ snd parsedBag

isBiggerThanAny :: Bag -> (Int, [Bag]) -> Bool
isBiggerThanAny bag parsedBag = any (`isBigger` bag) (snd parsedBag)

isBigger :: Bag -> Bag -> Bool
isBigger a b = red a > red b || green a > green b || blue a > blue b

toBag :: [(Int, String)] -> Bag
toBag xs =
  let singleBag x = case snd x of
        "red" -> Bag {red = fst x, green = 0, blue = 0}
        "blue" -> Bag {red = 0, green = 0, blue = fst x}
        "green" -> Bag {red = 0, green = fst x, blue = 0}
        _ -> mempty
   in mconcat $ singleBag <$> xs

-- Parsing
aocFile :: Parser [(Int, [Bag])]
aocFile = do
  x <- aocLine `endBy` char '\n'
  eof
  return x

aocLine :: Parser (Int, [Bag])
aocLine = do
  many space
  gi <- gameId
  many space
  bagi <- (bagNum `sepBy` char ',') `sepBy` char ';'
  return (gi, toBag <$> bagi)

gameId :: Parser Int
gameId = do
  many space
  string "Game "
  id <- many digit
  char ':'
  return (read id)

bagNum :: Parser (Int, String)
bagNum = do
  many space
  id <- many digit
  many space
  color <- many letter
  return (read id, toLower <$> color)

-- Bag Monoid
data Bag = Bag
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Show, Eq)

instance Semigroup Bag where
  a <> b = Bag {red = max (red a) (red b), green = max (green a) (green b), blue = max (blue a) (blue b)}

instance Monoid Bag where
  mempty = Bag {red = 0, green = 0, blue = 0}
