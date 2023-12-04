import Data.Char (digitToInt, toLower)
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
      let partValue x = case fst x of
            Part (v, _) -> v
            _ -> 0
      print $ sum $ partValue <$> partsWithSymbolsTouching (matrixToEngineList e)
      putStrLn "AOC Answer 2:"
      print $ sum $ gearsWithTwoPartsTouchingRatio $ matrixToEngineList e

data Engine = Period | Part (Int, Int) | Symbol Char deriving (Show, Eq)

-- (x, y coordinate system starting at 0)
data Position = Position {x :: Int, y :: Int} deriving (Show, Eq)

gearsWithTwoPartsTouchingRatio :: [(Engine, Position)] -> [Int]
gearsWithTwoPartsTouchingRatio a = gearRatios <$> filter (\x -> length x == 2) (getTouchingParts a <$> gears a)

gearRatios :: [(Engine, Position)] -> Int
gearRatios =
  let gearRatio b a = case fst b of
        Part (bVal, _) -> a * bVal
        _ -> 1
   in foldr gearRatio 1

partsWithSymbolsTouching :: [(Engine, Position)] -> [(Engine, Position)]
partsWithSymbolsTouching a =
  let partHasSymbolNeighbor sym = any (neighbor sym) (symbols a)
   in filter partHasSymbolNeighbor (parts a)

getTouchingParts :: [(Engine, Position)] -> (Engine, Position) -> [(Engine, Position)]
getTouchingParts arr el = filter (`neighbor` el) (parts arr)

gears :: [(Engine, Position)] -> [(Engine, Position)]
gears =
  let gear x = case fst x of
        Symbol x -> x == '*'
        _ -> False
   in filter gear

parts :: [(Engine, Position)] -> [(Engine, Position)]
parts =
  let part x = case fst x of
        Part (_, _) -> True
        _ -> False
   in filter part

symbols :: [(Engine, Position)] -> [(Engine, Position)]
symbols =
  let symbol x = case fst x of
        Symbol _ -> True
        _ -> False
   in filter symbol

neighbor :: (Engine, Position) -> (Engine, Position) -> Bool
neighbor a b =
  let xDist = (x . snd) b - (x . snd) a
      yDist = (y . snd) b - (y . snd) a
      maxLength = (itemLength . fst) a
   in (-1 <= xDist && xDist <= maxLength && -1 <= yDist && yDist <= 1)

matrixToEngineList :: [[Engine]] -> [(Engine, Position)]
matrixToEngineList mat = concat $ foldOuter $ foldInner <$> mat

foldOuter :: [[(Engine, Int)]] -> [[(Engine, Position)]]
foldOuter = snd . foldr aggEngineList (0, [])

aggEngineList :: [(Engine, Int)] -> (Int, [[(Engine, Position)]]) ->  (Int, [[(Engine, Position)]])
aggEngineList engPosList (ind, engPos) =
  let mappedList = createPosition ind <$> engPosList
   in (ind + 1, mappedList : engPos)

createPosition :: Int -> (Engine, Int) -> (Engine, Position)
createPosition y (engine, x) = (engine, Position {x = x, y = y})

foldInner :: [Engine] -> [(Engine, Int)]
foldInner =
  let aggEngines engine (index, list) =
        let accum = case engine of
              Period -> list
              Symbol x -> (Symbol x, index) : list
              Part (x, y) -> (Part (x, y), index) : list
         in (index + itemLength engine, accum)
   in snd . foldr aggEngines (0, [])

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