{-# LANGUAGE StrictData #-}

import Data.Char (digitToInt, toLower)
import Data.List (foldl')
import Data.Monoid
import Text.Parsec
import Text.Parsec.Text (Parser, parseFromFile)

-- Main function
main :: IO ()
main = do
  engines <- parseFromFile aocFile "aoc3.txt"
  case engines of
    Left err -> print err
    Right e -> do
      putStrLn "AOC3 Answer 1:"
      print $
        sum $
          fmap
            ( \x -> case fst x of
                Part (v, _) -> v
                _ -> 0
            )
            (partsWithSymbolsTouching $ matrixToEngineList e)
      putStrLn "AOC Answer 2:"
      print $ sum $ gearsWithTwoPartsTouchingRatio $ matrixToEngineList e

data Engine = Period | Part (Int, Int) | Symbol Char deriving (Show, Eq)

-- (x, y coordinate system starting at 0)
data Position = Position {x :: Int, y :: Int} deriving (Show, Eq)

gearsWithTwoPartsTouchingRatio :: [(Engine, Position)] -> [Int]
gearsWithTwoPartsTouchingRatio a = fmap gearRatios $ filter (\x -> length x == 2) $ getTouchingParts a <$> gears a

gearRatios :: [(Engine, Position)] -> Int
gearRatios =
  let gearRatio a b = case fst b of
        Part (bVal, _) -> a * bVal
        _ -> 1
   in foldl' gearRatio 1

partsWithSymbolsTouching :: [(Engine, Position)] -> [(Engine, Position)]
partsWithSymbolsTouching a =
  let partHasSymbolNeighbor sym = any (neighbor sym) (symbols a)
   in filter partHasSymbolNeighbor (parts a)

getTouchingParts :: [(Engine, Position)] -> (Engine, Position) -> [(Engine, Position)]
getTouchingParts arr el = filter (`neighbor` el) (parts arr)

gears :: [(Engine, Position)] -> [(Engine, Position)]
gears =
  filter
    ( \x -> case fst x of
        Symbol x -> x == '*'
        _ -> False
    )

parts :: [(Engine, Position)] -> [(Engine, Position)]
parts =
  filter
    ( \x -> case fst x of
        Part (_, _) -> True
        _ -> False
    )

symbols :: [(Engine, Position)] -> [(Engine, Position)]
symbols =
  filter
    ( \x -> case fst x of
        Symbol _ -> True
        _ -> False
    )

neighbor :: (Engine, Position) -> (Engine, Position) -> Bool
neighbor a b =
  let xDist = (x . snd) b - (x . snd) a
      yDist = (y . snd) b - (y . snd) a
      maxLength = (itemLength . fst) a
   in (-1 <= xDist && xDist <= maxLength && -1 <= yDist && yDist <= 1)

matrixToEngineList :: [[Engine]] -> [(Engine, Position)]
matrixToEngineList mat = concat $ foldOuter $ foldInner <$> mat

foldOuter :: [[(Engine, Int)]] -> [[(Engine, Position)]]
foldOuter = snd . foldl' aggEngineList (0, [])

aggEngineList :: (Int, [[(Engine, Position)]]) -> [(Engine, Int)] -> (Int, [[(Engine, Position)]])
aggEngineList (ind, engPos) engPosList =
  let mappedList = mapPositions ind <$> engPosList
   in (ind + 1, mappedList : engPos)

mapPositions :: Int -> (Engine, Int) -> (Engine, Position)
mapPositions y (engine, x) = (engine, Position {x = x, y = y})

foldInner :: [Engine] -> [(Engine, Int)]
foldInner =
  let aggEngines (index, list) engine = case engine of
        Period -> (index + 1, list)
        Symbol x -> (index + 1, (Symbol x, index) : list)
        Part (x, y) -> (index + y, (Part (x, y), index) : list)
   in snd . foldl' aggEngines (0, [])

itemLength :: Engine -> Int
itemLength engine = case engine of
  Period -> 1
  Part (_, x) -> x
  Symbol _ -> 1

aocFile :: Parser [[Engine]]
aocFile = aocLine `sepEndBy` endOfLine

aocLine :: Parser [Engine]
aocLine = many engine

engine :: Parser Engine
engine = period <|> part <|> symbol

period :: Parser Engine
period = char '.' >> return Period

part :: Parser Engine
part = do
  x <- many1 digit
  return $ Part (read x, length x)

symbol :: Parser Engine
symbol = Symbol <$> noneOf ".\n\r"