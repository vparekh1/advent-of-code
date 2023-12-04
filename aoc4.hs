import Control.Monad
import Control.Monad.ST
import Data.Char (digitToInt, toLower)
import Data.Monoid
import Data.Set
import Data.Vector.Mutable as M
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
      print $ sum $ pointTotal <$> e
      putStrLn "AOC4 Answer 2: "
      print $ pointTotal2 <$> e

data Card = Card {cardId :: Int, wins :: Set Int, mine :: [Int]} deriving (Show, Eq)

-- -- To use it, you need to run it within the ST monad
-- main :: IO ()
-- main = print $
--   runST $ do
--     v <- updateVector
--     M.freeze v -- Convert it back to an immutable vector

-- A function to update a mutable vector
updateVector :: [Int] -> ST s (M.STVector s Int)
updateVector pointTotals = do
  let pt = thaw . fromList . pointTotals
  v <- M.replicate 1 (length pt)
  V.iforM_ pt $ (\(i, x) -> )
  M.write v 0 10 -- Write 10 at index 0
  M.write v 1 20 -- Write 20 at index 1
  return v

pointTotal2 :: Card -> Int
pointTotal2 card = length $ Prelude.filter (\x -> x `member` wins card) (mine card)

pointTotal :: Card -> Double
pointTotal card =
  ( \x ->
      if Prelude.null x
        then 0.0
        else 2 ** (fromIntegral (length x) - 1)
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
  return $ Card {cardId = read cardId, wins = fromList wins, mine = mine}

numList :: Parser [Int]
numList = number `sepEndBy1` many1 (char ' ')

number :: Parser Int
number = do
  x <- many1 digit
  return $ read x
