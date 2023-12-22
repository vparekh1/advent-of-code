module Aoc202309 (solve) where

import Text.Parsec
import Text.Parsec.Text (Parser, parseFromFile)

solve :: IO ()
solve = do
  f <- parseFromFile aocFile "data/aoc9.txt"
  case f of
    Left err -> print err
    Right e -> do
        putStrLn "AOC9 Answer 1:"
        print $ sum $ (\x -> round $ lagrangeInterpolate (zip [1..] x) ((fromIntegral $ length $ x) + 1)) <$> e
        putStrLn "AOC9 Answer 2:"
        print $ sum $ (\x -> round $ lagrangeInterpolate (zip [1..] x) 0) <$> e

lagrangeInterpolate :: [(Double, Double)] -> Double -> Double
lagrangeInterpolate points x = sum [y * product [(x - xj) / (xi - xj) | (xj, _) <- points, xj /= xi] | (xi, y) <- points]

aocFile :: Parser [[Double]]
aocFile = aocLine `sepEndBy1` endOfLine

aocLine :: Parser [Double]
aocLine = do
    ints `sepEndBy1` many1 (char ' ')

ints :: Parser Double
ints = do
    x <- many1 (oneOf "1234567890-")
    return $ read x