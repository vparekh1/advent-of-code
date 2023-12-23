module Aoc202312 (solve) where

import Data.MemoTrie as T
import Parse (printSolution)
import Text.Parsec
import Text.Parsec.ByteString (Parser)

solve :: IO ()
solve = printSolution aocFile aocFile "data/aoc12.txt" (\g -> sum $ countPerm <$> g) (\g -> sum $ countPerm . quintuple <$> g)

type QueryItem = Char

type Query = [QueryItem]

type Answer = [Int]

countPerm :: (Query, Answer) -> Int
countPerm (query, answer) = countPerm' '.' query answer
  where
    recurse = T.memo3 countPerm'
    countPerm' _ [] [] = 1
    countPerm' _ [] (_ : _) = 0
    countPerm' _ qss [] = if '#' `elem` qss then 0 else 1
    countPerm' ch (q : qs) (a : as)
      | a + sum as + length as > length qs + 1 = 0
      | otherwise = case q of
          '.' -> recurse '.' qs (a : as)
          '#' ->
            let takes = '.' `notElem` take (a - 1) qs
                drops = drop (a - 1) qs
             in if ch /= '#' && length qs >= (a - 1) && takes then recurse '#' drops as else 0
          '?' -> recurse ch ('.' : qs) (a : as) + recurse ch ('#' : qs) (a : as)
          _ -> 0

quintuple :: (Query, Answer) -> (Query, Answer)
quintuple (q, a) =
  let qq = q ++ ['?']
   in (qq ++ qq ++ qq ++ qq ++ q, a ++ a ++ a ++ a ++ a)

aocFile :: Parser [(Query, Answer)]
aocFile = parseInteresting `sepEndBy1` endOfLine

parseInteresting :: Parser (Query, Answer)
parseInteresting = do
  query <- many1 (oneOf "?#.")
  spaces
  answer <- (read <$> many1 digit) `sepBy` char ','
  return (query, answer)
