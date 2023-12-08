import Data.Graph
import Data.Map
import Text.Parsec
import Data.Traversable
import Text.Parsec.Text (Parser, parseFromFile)

main :: IO ()
main = do
  f <- parseFromFile aocFile "aoc8.txt"
  case f of
    Left err -> print err
    Right e -> do
      putStrLn "AOC8 Answer 1:"
      print $ newMapWalk "AAA" "ZZZ" $ newMap e "ZZZ"
      putStrLn "AOC8 Answer 2:"
      print $ Prelude.foldl lcm 1 $ ($ newMap2 e "ZZZ") <$> (\x -> newMapWalk2 x "ZZZ") <$> (Prelude.filter (\x -> x !! 2 == 'A') $ keys $ snd e)

type Source = String
type Dest = String

newMapWalk2 :: Source -> Dest -> Map String (Int, String) ->  Int
newMapWalk2 source dest map = complexMapWalk map source dest 0
    where complexMapWalk map source dest count
            | source !! 2 == dest !! 2 = count
            | otherwise = let (dist, next) = map ! source
                        in complexMapWalk map next dest (count + dist)

newMap2 :: ([(String, String) -> String], Map String (String, String)) -> Dest -> Map String (Int, String)
newMap2 (funList, elfMap) dest = fromList $ (fullWalk2 (funList, elfMap) dest) <$> keys elfMap

fullWalk2 :: ([(String, String) -> String], Map String (String, String)) -> Source -> Dest -> (String, (Int, String))
fullWalk2 (funList, elfMap) dest source = complexWalk (funList, elfMap) source dest source 0
    where singleApply fun s = fun $ elfMap ! s
          complexWalk ([], _) source dest curr count = (source, (count, curr))
          complexWalk ((x:xs), elfMap) source dest curr count 
            | curr !! 2 == dest !! 2 = (source, (count, dest))
            | source !! 2 == dest !! 2 = (source, (0, source))
            | otherwise = let next = singleApply x curr
                        in complexWalk (xs, elfMap) source dest next (count + 1)

newMapWalk :: Source -> Dest -> Map String (Int, String) ->  Int
newMapWalk source dest map = complexMapWalk map source dest 0
    where complexMapWalk map source dest count
            | source == dest = count
            | otherwise = let (dist, next) = map ! source
                        in complexMapWalk map next dest (count + dist)

newMap :: ([(String, String) -> String], Map String (String, String)) -> Dest -> Map String (Int, String)
newMap (funList, elfMap) dest = fromList $ (fullWalk (funList, elfMap) dest) <$> keys elfMap

fullWalk :: ([(String, String) -> String], Map String (String, String)) -> Dest -> Source -> (String, (Int, String))
fullWalk (funList, elfMap) dest source = complexWalk (funList, elfMap) source dest source 0
    where singleApply fun s = fun $ elfMap ! s
          complexWalk ([], _) source dest curr count = (source, (count, curr))
          complexWalk ((x:xs), elfMap) source dest curr count 
            | curr == dest = (source, (count, dest))
            | source == dest = (source, (0, source))
            | otherwise = let next = singleApply x curr
                        in complexWalk (xs, elfMap) source dest next (count + 1)

aocFile :: Parser ([(String, String) -> String], Map String (String, String))
aocFile = do
    walk <- lrs
    spaces
    elfMap <- elf `sepEndBy1` endOfLine
    return $ (walk, fromList elfMap)

elf :: Parser (String, (String, String))
elf = do
    key <- many1 letter
    spaces >> char '=' >> spaces >> char '('
    left <- many1 letter
    spaces >> char ',' >> spaces
    right <- many1 letter
    char ')' >> many (char ' ')
    return $ (key, (left, right)) 

lrs :: Parser [(String, String) -> String]
lrs = do
    x <- many1 (oneOf "LR")
    return $ toBin <$> x
    where toBin b = case b of
            'L' -> fst 
            'R' -> snd
            _ -> snd
