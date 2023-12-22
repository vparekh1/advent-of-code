module Aoc202311 (solve) where
    
import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)
import Data.IntSet as S
import qualified Data.List as L
import Data.Vector.Unboxed as V

type Point = (Int, Int) 

solve :: IO ()
solve = do
  f <- parseFromFile aocFile "data/aoc11.txt"
  case f of
    Left err -> print err
    Right e -> do
        putStrLn "AOC11 Answer 1:"
        let emptyYs = emptyY e
            emptyXs = emptyX e
            dist = distance emptyXs emptyYs 2
        print $ Prelude.sum $ (uncurry dist) <$> (pairs $ fst <$> e)
        putStrLn "AOC11 Answer 2:"
        let dist2 = distance emptyXs emptyYs 1000000
        print $ Prelude.sum $ (uncurry dist2) <$> (pairs $ fst <$> e)

distance :: Vector Int -> Vector Int -> Int -> Point -> Point -> Int
distance emptyXs emptyYs expansion (x1,y1) (x2,y2) 
    = abs (y2 - y1) + abs (x2-x1) + (V.length $ V.filter (\x -> x >= (min x1 x2) && x <= (max x1 x2)) emptyXs) * (expansion-1)
                                    + (V.length $ V.filter (\x -> x >= (min y1 y2) && x <= (max y1 y2)) emptyYs) * (expansion-1)

emptys :: (Point -> Int) -> [(Point, Char)] -> Vector Int
emptys fun l = 
    let nonemptyYs = (fun . fst) <$> l
        maxY = Prelude.maximum nonemptyYs
    in V.fromList $ L.sort $ S.toList $ (S.fromList [1..maxY]) `difference` (S.fromList nonemptyYs)

emptyY :: [(Point, Char)] -> Vector Int
emptyY = emptys snd
emptyX :: [(Point, Char)] -> Vector Int
emptyX = emptys fst

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- L.tails l, y <- ys]

lolToMap :: (Num b1, Num a, Enum b1, Enum a) => [[b2]] -> [[((a, b1), b2)]]
lolToMap l = fmap (\x -> 
                let innerList = snd x
                    y = fst x
                    listMap = Prelude.zip [0..] innerList
                in [(((a,y), c)) | (a, c) <- listMap]) $ Prelude.zip [0..] l

aocFile :: Parser [(Point, Char)]
aocFile = do
    x <- aocLine `sepEndBy1` endOfLine
    eof
    return $ Prelude.filter (\c -> snd c == '#') $ Prelude.concat $ lolToMap x

aocLine :: Parser [Char]
aocLine = do
    many1 (noneOf "\n ")
