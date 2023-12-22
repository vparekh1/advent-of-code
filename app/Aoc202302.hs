module Aoc202302 (solve) where

import Data.Char (toLower)
import Text.Parsec
import Text.Parsec.Text (Parser, parseFromFile)

-- Main function
solve :: IO ()
solve =
  let elfBag = Bag {red = 12, green = 13, blue = 14}
   in do
        bagGames <- parseFromFile aocFile "data/aoc2.txt"
        case bagGames of
          Left err -> print err
          Right bag -> do
            putStrLn "First aoc2 problem: "
            print $ sum $ fst <$> filter (not . isBiggerThanAny elfBag) bag
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
  x <- aocLine `sepEndBy1` endOfLine
  eof
  return x

aocLine :: Parser (Int, [Bag])
aocLine = do
  gi <- gameId
  bagi <- (bagNum `sepEndBy1` char ',') `sepEndBy1` char ';'
  return (gi, toBag <$> bagi)

gameId :: Parser Int
gameId = do
  _ <- many space >> string "Game "
  i <- many digit
  _ <- char ':'
  return (read i)

bagNum :: Parser (Int, String)
bagNum = do
  _ <- many space
  i <- many digit
  _ <- many space
  color <- many letter
  return (read i, toLower <$> color)

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
