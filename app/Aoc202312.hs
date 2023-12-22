{-# LANGUAGE ViewPatterns #-}

module Aoc202312 (solve) where

import Data.HashMap.Strict (HashMap)
import Data.Vector.Unboxed as V
import Debug.Trace
import Parse (printSolution)
import Text.Parsec
import Text.Parsec.ByteString (Parser)

solve :: IO ()
solve = printSolution aocFile aocFile "data/aoc12test.txt" (fmap countPermutations) (const "")

type QueryItem = Char

type Query = V.Vector QueryItem

type Answer = V.Vector Int

countPermutations :: (Query, Answer) -> Int
countPermutations (V.uncons -> Nothing, V.uncons -> Nothing) = trace "221" 1
countPermutations (V.uncons -> Nothing, V.uncons -> Just (a, as))
  | a == 0 && V.null as = trace "241" 1
  | otherwise = trace "230" 0
countPermutations (V.uncons -> Just (q, qs), V.uncons -> Nothing)
  | q == '.' || q == '?' = traceShow (q `cons` qs, "25") countPermutations (qs, V.empty)
  | otherwise = traceShow (q `cons` qs, "260") 0
countPermutations (V.uncons -> Just (q, qs), V.uncons -> Just (a, as))
  | a + V.sum as + V.length as - 1 > V.length qs + 1 = traceShow (a `cons` as, q `cons` qs, "280") 0
  | a == 0 && V.null as = traceShow (a `cons` as, q `cons` qs, "29") countPermutations (q `cons` qs, V.empty)
  | q == '.' = case a of
      0 -> traceShow (a `cons` as, q `cons` qs, "31") countPermutations (qs, as)
      _ -> traceShow (a `cons` as, q `cons` qs, "32") countPermutations (qs, a `cons` as)
  | q == '#' = case a of
      0 -> traceShow (a `cons` as, q `cons` qs, "340") 0
      _ -> traceShow (a `cons` as, q `cons` qs, "35") countPermutations (qs, (a - 1) `cons` as)
  | q == '?' = countPermutations ('.' `cons` qs, a `cons` as) + countPermutations ('#' `cons` qs, a `cons` as)
  | otherwise = trace "WHATTTT" 0
countPermutations (_, _) = trace "THIS SHOULD NEVER HAPPEN" 0

-- countPermutations :: (Query, Answer) -> Int
-- countPermutations (V.uncons -> Nothing, V.uncons -> Nothing) = trace "22" 1
-- countPermutations (V.uncons -> Nothing, V.uncons -> Just _) = trace "23" 0
-- countPermutations (V.uncons -> Just (q, qs), V.uncons -> Nothing)
--   | q == '.' || q == '?' = traceShow (q `cons` qs, "25") countPermutations (qs, V.empty)
--   | otherwise = traceShow (q `cons` qs, "26") 0
-- countPermutations (V.uncons -> Just (q, qs), V.uncons -> Just (a, as))
--   | a + V.sum as + V.length as > V.length qs = traceShow (a `cons` as, q `cons` qs, "28") 0
--   | q == '.' = traceShow (a `cons` as, q `cons` qs, "29") countPermutations (qs, a `cons` as)
--   | q == '#' = traceShow (a `cons` as, q `cons` qs, "30") countPermutations nextHashQuery
--   | q == '?' = traceShow (a `cons` as, q `cons` qs, "31") countPermutations (qs, a `cons` as) + countPermutations nextHashQuery
--   | otherwise = trace "No way this would happen" 0
--   where
--     nextHashQuery
--       | a == 0 = (qs, as)
--       | a > 0 = (qs, (a - 1) `cons` as)
--       | otherwise = trace "WHYYY" (qs, as)
-- countPermutations (_, _) = trace "THIS SHOULD NEVER HAPPEN" 0

aocFile :: Parser [(Query, Answer)]
aocFile = parseInteresting `sepEndBy1` endOfLine

parseInteresting :: Parser (Query, Answer)
parseInteresting = do
  query <- many1 (oneOf "?#.")
  spaces
  answer <- (read <$> many1 digit) `sepBy` char ','
  return (V.fromList query, V.fromList answer)
