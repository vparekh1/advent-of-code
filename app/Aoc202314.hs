{-# LANGUAGE ViewPatterns #-}

module Aoc202314 (solve) where

import qualified Data.Maybe as Maybe
import Debug.Trace
import Parse
import Text.Parsec
import Text.Parsec.ByteString (Parser)

solve :: IO ()
solve = printSolution aocFile aocFile "data/aoc14.txt" (fmap (weight . rollUp)) (const "")

weight m = sum $ (uncurry (\x v -> (length $ filter (== 'O') v) * x)) <$> zip [1 ..] (reverse m)

rollRight m = rollRight1 <$> m

rollLeft m = (reverse . rollRight1 . reverse) <$> m

rollDown m = transpose $ rollRight1 <$> (transpose m)

rollUp m = transpose $ (reverse . rollRight1 . reverse) <$> (transpose m)

transpose :: [[a]] -> [[a]]
transpose v =
  [ [ (v !! col) !! row
      | col <- [0 .. maxRow]
    ]
    | row <- [0 .. maxCol]
  ]
  where
    maxRow = length v - 1
    maxCol = length (v !! 0) - 1

rollRight1 :: [Char] -> [Char]
rollRight1 [] = []
rollRight1 v =
  let untilHash = takeWhile (/= '#') v
      rem1 = dropWhile (/= '#') v
      hashes = takeWhile (== '#') rem1
      remainder = dropWhile (== '#') rem1
      dots = length $ filter (== '.') untilHash
      zeros = length $ filter (== 'O') untilHash
   in replicate dots '.' ++ replicate zeros 'O' ++ hashes ++ rollRight1 remainder

aocFile :: Parser [[[Char]]]
aocFile = parseGridList (noneOf " \n\r") (oneOf "#.O") `sepEndBy1` many1 endOfLine
