import Control.Monad
import Control.Monad.ST
import Data.Char (digitToInt, toLower)
import Data.Monoid
import Data.Set as Set
import Data.Vector.Mutable as M
import Data.Vector as V
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

-- Main function
main :: IO ()
main = do
  cards <- parseFromFile aocFile "aoc4.txt"
  case cards of
    Left err -> print err
    Right e -> do
      putStrLn "AOC4 Answer 1:"
      print $ Prelude.sum $ pointTotal <$> e
      putStrLn "AOC4 Answer 2: "
      print $ V.sum $ runST $ do 
                  v <- numberOfEachCard $ pointTotal2 <$> e
                  V.freeze v

data Card = Card {cardId :: Int, wins :: Set Int, mine :: [Int]} deriving (Show, Eq)

numberOfEachCard :: [Int] -> ST s (M.MVector s Int)
numberOfEachCard pointTotals = do
  pt <- thaw (V.fromList pointTotals)
  v <- M.replicate (M.length pt) 1
  M.iforM_ pt (\i val -> do
    increment <- M.read v i
    let lastIndex = M.length v - 1
        seqBeginning = min (i+1) lastIndex
        seqEnd = min (i + val) lastIndex
        updateList = if val == 0 || i == lastIndex
                     then []
                     else [seqBeginning..seqEnd]
    Control.Monad.forM_ updateList
      (\x -> M.modify v (+increment) x))
  return v

pointTotal2 :: Card -> Int
pointTotal2 card = Prelude.length $ Prelude.filter (\x -> x `member` wins card) (mine card)

pointTotal :: Card -> Double
pointTotal card =
  ( \x ->
      if Prelude.null x
        then 0.0
        else 2 ** (fromIntegral (Prelude.length x) - 1)
  )
    $ Prelude.filter (\x -> x `member` wins card) (mine card)

aocFile :: Parser [Card]
aocFile = aocLine `sepEndBy1` endOfLine

aocLine :: Parser Card
aocLine = do
  many space
  string' "Card"
  spaces
  cardId <- many1 digit
  char ':'
  spaces
  wins <- numList
  spaces
  char '|'
  spaces
  mine <- numList
  return $ Card {cardId = Prelude.read cardId, wins = Set.fromList wins, mine = mine}

numList :: Parser [Int]
numList = number `sepEndBy1` many1 (char ' ')

number :: Parser Int
number = do
  x <- many1 digit
  return $ Prelude.read x
