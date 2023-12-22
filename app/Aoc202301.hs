module Aoc202301 (solve) where

import Data.Char
import Parse
import Text.Parsec
  ( anyChar,
    digit,
    endOfLine,
    lookAhead,
    noneOf,
    sepEndBy1,
    string',
    try,
    (<|>),
  )
import Text.Parsec.ByteString (Parser, parseFromFile)

solve :: IO ()
solve =
  do
    f <- parseFromFile aocFile "data/aoc1.txt"
    case f of
      Left err -> print err
      Right g -> do
        putStrLn "AOC1:"
        print $ sum $ num1 <$> g
        putStrLn "AOC2:"
        print $ sum $ num2 <$> g

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
      (do Num . digitToInt <$> digit)
        <|> (do x <- string' "zero"; return $ Word x)
        <|> (do x <- string' "one"; return $ Word x)
        <|> (do x <- string' "two"; return $ Word x)
        <|> (do x <- string' "three"; return $ Word x)
        <|> (do x <- string' "four"; return $ Word x)
        <|> (do x <- string' "five"; return $ Word x)
        <|> (do x <- string' "six"; return $ Word x)
        <|> (do x <- string' "seven"; return $ Word x)
        <|> (do x <- string' "eight"; return $ Word x)
        <|> (do x <- string' "nine"; return $ Word x)
  _ <- anyChar
  return x
