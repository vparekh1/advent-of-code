module Aoc202314 (solve) where

import Data.Map as M
import Parse
import Text.Parsec
import Text.Parsec.ByteString (Parser)

solve :: IO ()
solve = printSolution aocFile aocFile "data/aoc14.txt" (fmap (weight . rollUp)) (fmap $ weight . mspin 1000000000)

mspin :: Int -> [[Char]] -> [[Char]]
mspin = go M.empty
  where
    go set count m
      | count == 0 = m
      | otherwise = case set M.!? m of
          Nothing -> go (M.insert m count set) (count - 1) (spin m)
          Just x -> mspin (count `rem` (x - count)) m

spin :: [[Char]] -> [[Char]]
spin = rollRight . rollDown . rollLeft . rollUp

weight :: [[Char]] -> Int
weight m = sum $ (\(x, v) -> length (Prelude.filter (== 'O') v) * x) <$> zip [1 ..] (reverse m)

rollRight :: [[Char]] -> [[Char]]
rollRight m = rollRight1 <$> m

rollLeft :: [[Char]] -> [[Char]]
rollLeft m = reverse . rollRight1 . reverse <$> m

rollDown :: [[Char]] -> [[Char]]
rollDown m = transpose $ rollRight1 <$> transpose m

rollUp :: [[Char]] -> [[Char]]
rollUp m = transpose $ reverse . rollRight1 . reverse <$> transpose m

transpose :: [[a]] -> [[a]]
transpose v =
  [ [ v !! col !! row
      | col <- [0 .. maxRow]
    ]
    | row <- [0 .. maxCol]
  ]
  where
    maxRow = length v - 1
    maxCol = length (head v) - 1

rollRight1 :: String -> String
rollRight1 [] = []
rollRight1 v =
  let untilHash = takeWhile (/= '#') v
      rem1 = dropWhile (/= '#') v
      hashes = takeWhile (== '#') rem1
      remainder = dropWhile (== '#') rem1
      dots = length $ Prelude.filter (== '.') untilHash
      zeros = length $ Prelude.filter (== 'O') untilHash
   in replicate dots '.' ++ replicate zeros 'O' ++ hashes ++ rollRight1 remainder

aocFile :: Parser [[String]]
aocFile = parseGridList (noneOf " \n\r") (oneOf "#.O") `sepEndBy1` many1 endOfLine
