module Aoc202315 (solve) where

import Data.Char (isDigit, ord)
import qualified Data.Vector as V
import Parse
import Text.Parsec
import Text.Parsec.ByteString (Parser)

solve :: IO ()
solve = printSolution aocFile aocFile "data/aoc15.txt" (\g -> sum $ hash <$> g) (\g -> ans2 $ readComm <$> g)

data Op = Dash String | Equal String Int deriving (Show, Eq)

ans2 :: [(Int, Op)] -> Int
ans2 g = sum $ uncurry focus <$> V.zip (V.fromList [1 .. 256]) (foldl applyOp (V.replicate 256 []) g)

focus :: Int -> [(String, Int)] -> Int
focus box contents = box * sum ((\x -> fst x * (snd . snd) x) <$> zip [1 ..] contents)

applyOp :: V.Vector [(String, Int)] -> (Int, Op) -> V.Vector [(String, Int)]
applyOp xs (hash, op) = case op of
  Dash s -> xs `V.update` V.fromList [(hash, filter (\x -> fst x /= s) (xs V.! hash))]
  Equal s i ->
    let l = xs V.! hash
        isThere = any (\x -> fst x == s) l
     in if isThere
          then xs `V.update` V.fromList [(hash, (\x -> if fst x == s then (s, i) else x) <$> l)]
          else xs `V.update` V.fromList [(hash, l ++ [(s, i)])]

fromString :: String -> Op
fromString s =
  let name = takeWhile (\x -> x /= '-' && x /= '=') s
   in if '=' `elem` s then Equal name (read (dropWhile (not . isDigit) s)) else Dash name

readComm :: String -> (Int, Op)
readComm s = case fromString s of
  Dash s -> (hash s, Dash s)
  Equal s i -> (hash s, Equal s i)

hash :: String -> Int
hash = go 0
  where
    go acc [] = acc
    go acc (x : xs) = go ((acc + ord x) * 17 `rem` 256) xs

aocFile :: Parser [String]
aocFile = many1 (noneOf ",") `sepEndBy1` char ','
