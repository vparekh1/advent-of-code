module Aoc202301 (solve) where

import Data.Char
import Parse
import Text.Parsec
import Text.Parsec.ByteString (Parser)

solve :: IO ()
solve = printSolution aocFile aocFile "data/aoc1.txt" (\g -> sum $ num1 <$> g) (\g -> sum $ num2 <$> g)

data Interesting = Num Int | Word String deriving (Eq, Show)

fromInteresting :: Interesting -> Int
fromInteresting x = case x of
  Num n -> n
  Word n -> word2num n

num1 :: [Interesting] -> Int
num1 l = fromInteresting (head [x | x@(Num _) <- l]) * 10 + fromInteresting (last [x | x@(Num _) <- l])

num2 :: [Interesting] -> Int
num2 l = fromInteresting (head l) * 10 + fromInteresting (last l)

aocFile :: Parser [[Interesting]]
aocFile =
  (do x <- parseLineList (noneOf " \n\r") (try parseInteresting); return (snd <$> x)) `sepEndBy1` endOfLine

parseInteresting :: Parser Interesting
parseInteresting = do
  x <-
    lookAhead $
      (Num . digitToInt <$> digit)
        <|> (Word <$> string' "zero")
        <|> (Word <$> string' "one")
        <|> (Word <$> string' "two")
        <|> (Word <$> string' "three")
        <|> (Word <$> string' "four")
        <|> (Word <$> string' "five")
        <|> (Word <$> string' "six")
        <|> (Word <$> string' "seven")
        <|> (Word <$> string' "eight")
        <|> (Word <$> string' "nine")
  _ <- anyChar
  return x
