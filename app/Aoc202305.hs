{-# LANGUAGE ViewPatterns #-}

module Aoc202305 (solve) where

import Data.Char (digitToInt, toLower)
import Data.Foldable as F
import Data.Monoid
import Data.Vector as V
import Text.Parsec
import Text.Parsec.Text (Parser, parseFromFile)

data Smap = Smap {dest :: !Int, source :: !Int, range :: !Int} deriving (Show, Eq)

solve :: IO ()
solve = do
  f <- parseFromFile aocFile "data/aoc5.txt"
  case f of
    Left err -> print err
    Right e -> do
      putStrLn "AOC5 Answer 1:"
      print $ V.minimum $ foldLocation (snd e) <$> fst e
  g <- parseFromFile aocFile2 "data/aoc5.txt"
  case g of
    Left err -> print err
    Right e -> do
      putStrLn "AOC5 Answer 2:"
      print $ F.minimum $ fst <$> fst (F.foldl singleLayer (fst e, []) (snd e))

singleLayer :: ([(Int, Int)], [(Int, Int)]) -> [Smap] -> ([(Int, Int)], [(Int, Int)])
singleLayer seedList smap =
  let a = F.foldl foldingRanges seedList smap
   in (uncurry (Prelude.++) a, [])

foldingRanges :: ([(Int, Int)], [(Int, Int)]) -> Smap -> ([(Int, Int)], [(Int, Int)])
foldingRanges x smap = combineArrays [([], snd x), rangeArray (fst x) smap]

rangeArray :: [(Int, Int)] -> Smap -> ([(Int, Int)], [(Int, Int)])
rangeArray seedList smap = combineArrays $ (`rangeLocation` smap) <$> seedList

combineArrays :: [([(Int, Int)], [(Int, Int)])] -> ([(Int, Int)], [(Int, Int)])
combineArrays x = (F.concat $ fst <$> x, F.concat $ snd <$> x)

rangeLocation :: (Int, Int) -> Smap -> ([(Int, Int)], [(Int, Int)])
rangeLocation seedPair smap
  | s1 < d1 && d2 < s2 = ([(s1, d1 - s1), (d2 + 1, s2 - d2)], [lmap (d1, d2)])
  | s1 >= d1 && s2 <= d2 = ([], [lmap (s1, s2)])
  | s1 >= d1 && s1 <= d2 && s2 >= d2 = ([(d2 + 1, s2 - d2)], [lmap (s1, d2)])
  | s1 < d1 && s2 >= d1 && s2 <= d2 = ([(s1, d1 - s1)], [lmap (d1, s2)])
  | otherwise = ([(s1, s2 - s1 + 1)], [])
  where
    s1 = fst seedPair
    s2 = uncurry (+) seedPair - 1
    d1 = source smap
    d2 = source smap + range smap - 1
    lmap (a, b) = (locationMap smap a, b - a + 1)

locationMap :: Smap -> Int -> Int
locationMap x seed =
  let sourceDiff = seed - source x
      b = sourceDiff >= 0 && sourceDiff < range x
   in (if b then dest x + sourceDiff else seed)

foldLocation :: Vector (Vector Smap) -> Int -> Int
foldLocation (V.uncons -> Nothing) seed = seed
foldLocation (V.uncons -> Just (x, xs)) seed = foldLocation xs (oneLocation seed x)

oneLocation :: Int -> Vector Smap -> Int
oneLocation seed (V.uncons -> Nothing) = seed
oneLocation seed (V.uncons -> Just (x, xs)) =
  let sourceDiff = seed - source x
      b = sourceDiff >= 0 && sourceDiff < range x
   in (if b then dest x + sourceDiff else oneLocation seed xs)

-- Parsers
aocFile :: Parser (Vector Int, Vector (Vector Smap))
aocFile = do
  see <- seeds
  many1 (noneOf "0123456789")
  sm <- manySmaps
  return (see, sm)

aocFile2 :: Parser ([(Int, Int)], [[Smap]])
aocFile2 = do
  see <- seeds2
  many1 (noneOf "0123456789")
  sm <- manySmaps
  return (V.toList see, V.toList $ V.toList <$> sm)

manySmaps :: Parser (Vector (Vector Smap))
manySmaps = do
  x <- readSmaps `sepEndBy1` many1 (noneOf "0123456789")
  return (V.fromList x)

readSmaps :: Parser (Vector Smap)
readSmaps = do
  x <- readSmap `sepEndBy1` endOfLine
  return (V.fromList x)

readSmap :: Parser Smap
readSmap = do
  d <- many1 digit
  spaces
  s <- many1 digit
  spaces
  r <- many1 digit
  return (Smap {dest = read d, source = read s, range = read r})

seeds :: Parser (Vector Int)
seeds = do
  string' "seeds:"
  spaces
  x <- seed `sepBy` many1 (char ' ')
  return (V.fromList x)

seeds2 :: Parser (Vector (Int, Int))
seeds2 = do
  string' "seeds:"
  spaces
  x <- seed2 `sepBy` many1 (char ' ')
  return (V.fromList x)

seed2 :: Parser (Int, Int)
seed2 = do
  x <- many1 digit
  spaces
  y <- many1 digit
  return (read x, read y)

seed :: Parser Int
seed = do
  x <- many1 digit
  return (read x)
