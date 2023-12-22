{-# LANGUAGE ViewPatterns #-}

module Aoc202312 (solve) where

import Data.HashMap.Strict (HashMap)
import Data.Vector.Unboxed as V
import Debug.Trace
import Parse (printSolution)
import Text.Parsec
import Text.Parsec.ByteString (Parser)

solve :: IO ()
solve = printSolution aocFile aocFile "data/aoc12test.txt" (\g -> countPermutations <$> g) (const "")

type QueryItem = Char

type Query = V.Vector QueryItem

type Answer = V.Vector Int

countPermutations :: (Query, Answer) -> Int
countPermutations (V.uncons -> Nothing, V.uncons -> Nothing) = 1
countPermutations (V.uncons -> Nothing, V.uncons -> Just (a, as))
  | a == 0 && V.null as = 1
  | otherwise = 0
countPermutations (V.uncons -> Just (q, qs), V.uncons -> Nothing)
  | q == '.' || q == '?' = countPermutations (qs, V.empty)
  | otherwise = 0
countPermutations (V.uncons -> Just (q, qs), V.uncons -> Just (a, as))
  | a + V.sum as + V.length as - 1 > V.length qs + 1 = 0
  | a == 0 && V.null as = countPermutations (q `cons` qs, V.empty)
  | q == '.' = case a of
      0 -> countPermutations (qs, as)
      _ -> countPermutations (qs, fulla)
  | q == '#' = case a of
      0 -> 0
      _ -> countPermutations (qs, (a - 1) `cons` as)
  | q == '?' = countPermutations ('.' `cons` qs, fulla) + countPermutations ('#' `cons` qs, fulla)
  | otherwise = 0
  where
    fulla = a `cons` as
countPermutations (_, _) = 0

aocFile :: Parser [(Query, Answer)]
aocFile = parseInteresting `sepEndBy1` endOfLine

parseInteresting :: Parser (Query, Answer)
parseInteresting = do
  query <- many1 (oneOf "?#.")
  spaces
  answer <- (read <$> many1 digit) `sepBy` char ','
  return (V.fromList query, V.fromList answer)
