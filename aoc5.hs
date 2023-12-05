import Data.Char (digitToInt, toLower)
import Data.Monoid
import Text.Parsec
import Text.Parsec.Text (Parser, parseFromFile)
import Data.Foldable as F

data Smap = Smap {dest :: !Int, source :: !Int, range :: !Int} deriving (Show, Eq)

main :: IO ()
main = do
  f <- parseFromFile aocFile "aoc5.txt"
  case f of
    Left err -> print err
    Right e -> do
        putStrLn "AOC5 Answer 1:"
        print $ minimum $ (foldLocation (snd e)) <$> (fst e)
  g <- parseFromFile aocFile2 "aoc5.txt"
  case g of
    Left err -> print err
    Right e -> do
        putStrLn "AOC5 Answer 2:"
        print $ minimum $ fst <$> fst (foldl singleLayer (fst e, []) (snd e))

singleLayer :: ([(Int, Int)], [(Int, Int)]) -> [Smap] -> ([(Int, Int)], [(Int, Int)])
singleLayer seedList smap = 
    let a = foldl foldingRanges seedList smap
    in ((fst a) ++ (snd a), [])

foldingRanges :: ([(Int, Int)], [(Int, Int)]) -> Smap -> ([(Int, Int)], [(Int, Int)])
foldingRanges x smap = combineArrays [([], snd x), rangeArray (fst x) smap]

rangeArray :: [(Int, Int)] -> Smap -> ([(Int, Int)], [(Int, Int)])
rangeArray seedList smap = combineArrays $ (\x -> rangeLocation x smap) <$> seedList

combineArrays :: [([(Int, Int)], [(Int, Int)])] -> ([(Int, Int)], [(Int, Int)])
combineArrays x = (concat $ fst <$> x, concat $ snd <$> x)

rangeLocation :: (Int, Int) -> Smap -> ([(Int, Int)], [(Int, Int)])
rangeLocation seedPair smap
        | s1 < d1 && d2 < s2 = ([(s1, d1-s1), (d2+1, s2-d2)], [lmap (d1, d2)])
        | s1 >= d1 && s2 <= d2 = ([], [lmap (s1, s2)])
        | s1 >= d1 && s1 <= d2 && s2 >= d2 = ([(d2+1, s2-d2)], [lmap (s1, d2)])
        | s1 < d1 && s2 >= d1 && s2 <= d2 = ([(s1, d1-s1)], [lmap (d1, s2)])
        | otherwise = ([(s1, s2-s1+1)], [])
    where s1 = fst seedPair
          s2 = fst seedPair + snd seedPair - 1
          d1 = source smap
          d2 = source smap + range smap - 1
          lmap (a, b) = (locationMap smap a, b-a+1)
    
locationMap :: Smap -> Int -> Int
locationMap x seed = 
    let sourceDiff = seed - source x
        b = sourceDiff >= 0 && sourceDiff < (range x)
    in case b of 
        True -> dest x + sourceDiff
        False -> seed

foldLocation :: [[Smap]] -> Int -> Int
foldLocation [] seed = seed
foldLocation (x:xs) seed = foldLocation xs (oneLocation seed x)

oneLocation :: Int -> [Smap] -> Int
oneLocation seed [] = seed
oneLocation seed (x:xs) = 
    let sourceDiff = seed - source x
        b = sourceDiff >= 0 && sourceDiff < (range x)
    in case b of 
        True -> dest x + sourceDiff
        False -> oneLocation seed xs

-- Parsers
aocFile :: Parser ([Int], [[Smap]])
aocFile = do
    see <- seeds
    many1 (noneOf "0123456789")
    sm <- manySmaps
    return (see, sm)

aocFile2 :: Parser ([(Int, Int)], [[Smap]])
aocFile2 = do
    see <- seeds2
    many1 (noneOf "0123456789")
    sm <- manySmaps
    return (see, sm)

manySmaps :: Parser [[Smap]]
manySmaps = do
    readSmaps `sepEndBy1` many1 (noneOf "0123456789")

readSmaps :: Parser [Smap]
readSmaps = do
    readSmap `sepEndBy1` endOfLine

readSmap :: Parser Smap
readSmap = do
    d <- many1 digit
    spaces
    s <- many1 digit
    spaces
    r <- many1 digit
    return (Smap {dest = read d, source = read s, range = read r})

seeds :: Parser [Int]
seeds = do
    string' "seeds:"
    spaces
    seed `sepBy` many1 (char ' ')

seeds2 :: Parser [(Int, Int)]
seeds2 = do
    string' "seeds:"
    spaces
    seed2 `sepBy` many1 (char ' ')

seed2 :: Parser (Int, Int)
seed2 = do
    x <- many1 digit
    spaces
    y <- many1 digit
    return (read x,read y)

seed :: Parser Int
seed = do 
    x <- many1 digit
    return (read x)
