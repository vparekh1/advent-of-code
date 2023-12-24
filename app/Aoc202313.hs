module Aoc202313 (solve) where

import qualified Data.Maybe as Maybe
import Data.Vector as V
import Parse
import Text.Parsec
import Text.Parsec.ByteString (Parser)

type Matrix = Vector (Vector Char)

solve :: IO ()
solve = printSolution aocFile aocFile "data/aoc13.txt" (ans isReflect) (ans offByOneReflect)

ans :: (Vector (Vector Char) -> Int -> Bool) -> [Matrix] -> Int
ans singleReflect g =
  let reflection m = V.filter (singleReflect m) (V.fromList [1 .. (V.length m - 1)]) V.!? 0
      vertList = reflection . transpose <$> g
      horizList = reflection <$> g
      vertNums = Maybe.catMaybes vertList
      horizNums = Maybe.catMaybes horizList
   in Prelude.sum vertNums + Prelude.sum horizNums * 100

transpose :: Matrix -> Matrix
transpose v =
  V.fromList
    [ V.fromList
        [ (v V.! col) V.! row
          | col <- [0 .. maxRow]
        ]
      | row <- [0 .. maxCol]
    ]
  where
    maxRow = V.length v - 1
    maxCol = V.length (v V.! 0) - 1

offByOneReflect :: Matrix -> Int -> Bool
offByOneReflect mat n = (go 0 (n - 1) n mat) == 1
  where
    maxRow = V.length mat - 1
    go :: Int -> Int -> Int -> Matrix -> Int
    go acc low high m
      | low < 0 || high > maxRow = acc
      | otherwise = go (acc + mismatches) (low - 1) (high + 1) m
      where
        mismatches = V.length $ V.filter (uncurry (/=)) (V.zip (m V.! low) (m V.! high))

isReflect :: Matrix -> Int -> Bool
isReflect mat n = go (n - 1) n mat
  where
    maxRow = V.length mat - 1
    go low high m
      | low < 0 || high > maxRow = True
      | m V.! low == m V.! high = go (low - 1) (high + 1) m
      | otherwise = False

aocFile :: Parser [Matrix]
aocFile = parseGridVector (noneOf " \n\r") (oneOf ".#") `sepEndBy1` many1 endOfLine
