import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)
import Data.HashSet as S
import Data.List

type Point = (Int, Int) 

main :: IO ()
main = do
  f <- parseFromFile aocFile "aoc11.txt"
  case f of
    Left err -> print err
    Right e -> do
        putStrLn "AOC11 Answer 1:"
        let emptyYs = emptyY e
            emptyXs = emptyX e
            dist = distance emptyXs emptyYs 2
        print $ sum $ (\x -> dist (fst x) (snd x)) <$> (pairs $ fst <$> e)
        putStrLn "AOC11 Answer 2:"
        let dist2 = distance emptyXs emptyYs 1000000
        print $ sum $ (\x -> dist2 (fst x) (snd x)) <$> (pairs $ fst <$> e)

distance :: HashSet Int -> HashSet Int -> Int -> Point -> Point -> Int
distance emptyXs emptyYs expansion (x1,y1) (x2,y2) 
    = abs (y2 - y1) + abs (x2-x1) + (length $ S.filter (\x -> x >= (min x1 x2) && x <= (max x1 x2)) emptyXs) * (expansion-1)
                                    + (length $ S.filter (\x -> x >= (min y1 y2) && x <= (max y1 y2)) emptyYs) * (expansion-1)

emptys :: (Point -> Int) -> [(Point, Char)] -> HashSet Int
emptys fun l = 
    let nonemptyYs = (fun . fst) <$> l
        maxY = maximum nonemptyYs
    in (S.fromList [1..maxY]) `difference` (S.fromList nonemptyYs)

emptyY :: [(Point, Char)] -> HashSet Int
emptyY = emptys snd
emptyX :: [(Point, Char)] -> HashSet Int
emptyX = emptys fst

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

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
    return $ Prelude.filter (\x -> snd x == '#') $ Prelude.concat $ lolToMap x

aocLine :: Parser [Char]
aocLine = do
    many1 (noneOf "\n ")
