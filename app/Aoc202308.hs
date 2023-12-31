{-# LANGUAGE ViewPatterns #-}

module Aoc202308 (solve) where

import Data.ByteString as B
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.HashMap.Strict as M
import Data.Vector as V
import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)

solve :: IO ()
solve = do
  f <- parseFromFile aocFile "data/aoc8.txt"
  case f of
    Left err -> print err
    Right e -> do
      putStrLn "AOC8 Answer 1:"
      let aaa = pack $ BS.c2w <$> "AAA"
          zzz = pack $ BS.c2w <$> "ZZZ"
      print $ newHashMapWalk aaa zzz $ newHashMap e zzz
      putStrLn "AOC8 Answer 2:"
      print $
        Prelude.foldr
          ( lcm
              . ( ($ newHashMap2 e zzz)
                    <$> (`newHashMapWalk` zzz)
                )
          )
          1
          (Prelude.filter (\x -> x B.!? 2 == Just (BS.c2w 'A')) $ keys $ snd e)

newHashMap2 :: (Vector ((ByteString, ByteString) -> ByteString), HashMap ByteString (ByteString, ByteString)) -> ByteString -> HashMap ByteString (Int, ByteString)
newHashMap2 (funList, elfHashMap) dest = M.fromList $ fullWalk2 (funList, elfHashMap) dest <$> keys elfHashMap

fullWalk2 :: (Vector ((ByteString, ByteString) -> ByteString), HashMap ByteString (ByteString, ByteString)) -> ByteString -> ByteString -> (ByteString, (Int, ByteString))
fullWalk2 (funList, elfHashMap) dest source = complexWalk (funList, elfHashMap) source dest source 0
  where
    singleApply fun s = fun $ elfHashMap M.! s
    complexWalk (V.uncons -> Nothing, _) source dest curr count = (source, (count, curr))
    complexWalk (V.uncons -> Just (x, xs), elfHashMap) source dest curr count
      | curr B.!? 2 == dest B.!? 2 = (source, (count, dest))
      | otherwise =
          let next = singleApply x curr
           in complexWalk (xs, elfHashMap) source dest next (count + 1)

newHashMapWalk :: ByteString -> ByteString -> HashMap ByteString (Int, ByteString) -> Int
newHashMapWalk source dest map = complexHashMapWalk map source dest 0
  where
    complexHashMapWalk map source dest count
      | source == dest = count
      | otherwise =
          let (dist, next) = map M.! source
           in complexHashMapWalk map next dest (count + dist)

newHashMap :: (Vector ((ByteString, ByteString) -> ByteString), HashMap ByteString (ByteString, ByteString)) -> ByteString -> HashMap ByteString (Int, ByteString)
newHashMap (funList, elfHashMap) dest = M.fromList $ fullWalk (funList, elfHashMap) dest <$> keys elfHashMap

fullWalk :: (Vector ((ByteString, ByteString) -> ByteString), HashMap ByteString (ByteString, ByteString)) -> ByteString -> ByteString -> (ByteString, (Int, ByteString))
fullWalk (funList, elfHashMap) dest source = complexWalk (funList, elfHashMap) source dest source 0
  where
    singleApply fun s = fun $ elfHashMap M.! s
    complexWalk (V.uncons -> Nothing, _) source dest curr count = (source, (count, curr))
    complexWalk (V.uncons -> Just (x, xs), elfHashMap) source dest curr count
      | curr == dest = (source, (count, dest))
      | otherwise =
          let next = singleApply x curr
           in complexWalk (xs, elfHashMap) source dest next (count + 1)

aocFile :: Parser (Vector ((ByteString, ByteString) -> ByteString), HashMap ByteString (ByteString, ByteString))
aocFile = do
  walk <- lrs
  spaces
  elfHashMap <- elf `sepEndBy1` endOfLine
  eof
  return (walk, M.fromList elfHashMap)

elf :: Parser (ByteString, (ByteString, ByteString))
elf = do
  key <- many1 letter
  many1 (oneOf " =(")
  left <- many1 letter
  many1 (oneOf " ,")
  right <- many1 letter
  many1 (oneOf " )")
  return (pack $ BS.c2w <$> key, (pack $ BS.c2w <$> left, pack $ BS.c2w <$> right))

lrs :: Parser (Vector ((ByteString, ByteString) -> ByteString))
lrs = do
  x <- many1 (oneOf "LR")
  return $ V.fromList $ toBin <$> x
  where
    toBin b = case b of
      'L' -> fst
      'R' -> snd
      _ -> snd
