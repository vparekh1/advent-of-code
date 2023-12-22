{-# LANGUAGE ViewPatterns #-}

module Aoc202306 (solve) where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

solve :: IO ()
solve = do
  f <- parseFromFile aocFile "data/aoc6.txt"
  case f of
    Left err -> print err
    Right e -> do
      putStrLn "AOC6 Answer 1:"
      print $ product $ numOfOptions <$> e
  g <- parseFromFile aocFile2 "data/aoc6.txt"
  case g of
    Left err -> print err
    Right e -> do
      putStrLn "AOC6 Answer 2: "
      print $ numOfOptions $ e

numOfOptions :: (Double, Double) -> Int
numOfOptions (t,d) = 
    let low = floor $ 1 + (t - sqrt (t*t - 4*d))/2
        high = ceiling $ -1 + (t + sqrt (t*t - 4*d))/2
    in high - low + 1

aocFile :: Parser [(Double, Double)]
aocFile = do
    string' "Time:" >> spaces
    t <- many1 digit `sepEndBy` (many1 (char ' '))
    endOfLine >> string' "Distance:" >> spaces
    d <- many1 digit `sepEndBy` (many1 (char ' '))
    return $ zip (read <$> t) (read <$> d)

aocFile2 :: Parser (Double, Double)
aocFile2 = do
    string' "Time:" >> spaces
    t <- many1 digit `sepEndBy` (many1 (char ' '))
    endOfLine >> string' "Distance:" >> spaces
    d <- many1 digit `sepEndBy` (many1 (char ' '))
    return $ ((read $ concat t), (read $ concat d))