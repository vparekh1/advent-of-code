import Data.List
import Text.Parsec
import Text.Parsec.Text (Parser, parseFromFile)

main :: IO ()
main = do
  f <- parseFromFile aocFile "aoc7.txt"
  case f of
    Left err -> print err
    Right e -> do
      putStrLn "AOC3 Answer 1:"
      print $ sum $ mapHands <$> (zip [1..] $ sort $ e)
  g <- parseFromFile aocFile2 "aoc7.txt"
  case g of
    Left err -> print err
    Right e -> do
      putStrLn "AOC3 Answer 2:"
      print $ sum $ mapHands <$> (zip [1..] $ sort $ e)

data Hand = Hand {card :: Rank, bet :: Int} deriving (Show, Eq, Ord)

mapHands :: (Int, Hand) -> Int
mapHands (i, h) = i * (bet h)

order2 :: Char -> Int
order2 'J' = -1
order2 '2' = 2
order2 '3' = 3
order2 '4' = 4
order2 '5' = 5
order2 '6' = 6
order2 '7' = 7
order2 '8' = 8
order2 '9' = 9
order2 'T' = 10
order2 'Q' = 12
order2 'K' = 13
order2 'A' = 14

order :: Char -> Int
order '2' = 2
order '3' = 3
order '4' = 4
order '5' = 5
order '6' = 6
order '7' = 7
order '8' = 8
order '9' = 9
order 'T' = 10
order 'J' = 11
order 'Q' = 12
order 'K' = 13
order 'A' = 14

data Rank = HighCard [Int] 
            | OnePair [Int]  
            | TwoPair [Int] 
            | ThreeOfAKind [Int]  
            | FullHouse [Int]  
            | FourOfAKind [Int]  
            | FiveOfAKind [Int] deriving (Show, Eq, Ord)

groups :: String -> [Int]
groups s = sort $ length <$> group (sort s)

jokers :: String -> Int
jokers s = length $ filter (=='J') s

fromString2 :: String -> Rank
fromString2 s 
    | 5 `elem` g || maxelem + j == 5 = FiveOfAKind x
    | 4 `elem` g || maxelem + j == 4 =  FourOfAKind x
    | 3 `elem` g && 2 `elem` g || g == [1,2,2] && j >= 1 = FullHouse x
    | 3 `elem` g || maxelem + j == 3 = ThreeOfAKind x
    | g == [1,2,2] || g == [1,1,1,2] && j >= 1 = TwoPair x
    | 2 `elem` g || g == [1,1,1,1,1] && j >= 1 = OnePair x
    | otherwise = HighCard x
    where g = groups s 
          x = order2 <$> s
          j = jokers s
          maxelem = maximum $ groups $ filter (/='J') $ s

fromString :: String -> Rank
fromString s 
    | 5 `elem` g = FiveOfAKind x
    | 4 `elem` g =  FourOfAKind x
    | 3 `elem` g && 2 `elem` g = FullHouse x
    | 3 `elem` g = ThreeOfAKind x
    | g == [1,2,2] = TwoPair x
    | 2 `elem` g = OnePair x
    | otherwise = HighCard x
    where g = groups s 
          x = order <$> s

aocFile :: Parser [Hand]
aocFile = do
    hand `sepEndBy1` endOfLine

hand :: Parser Hand
hand = do
    card <- many1 (oneOf "23456789TAJQK")
    many1 (char ' ')
    bet <- many1 digit
    return Hand {card=fromString card, bet=read bet}

aocFile2 :: Parser [Hand]
aocFile2 = do
    hand2 `sepEndBy1` endOfLine

hand2 :: Parser Hand
hand2 = do
    card <- many1 (oneOf "23456789TAJQK")
    many1 (char ' ')
    bet <- many1 digit
    return Hand {card=fromString2 card, bet=read bet}

