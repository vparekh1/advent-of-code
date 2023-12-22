module Parse
  ( parseLineList,
    parseLineMap,
    parseGrid,
    numbersAsWords,
    word2num,
    num2word,
    parseLineVector,
    printSolution,
    Point,
  )
where

import Data.HashMap.Strict as M
import Data.IntMap as I
import Data.Maybe as Maybe
import Data.Vector as V
import Text.Parsec
import Text.Parsec.ByteString

printSolution :: (Show a1, Show a2) => Parser t1 -> Parser t2 -> FilePath -> (t1 -> a1) -> (t2 -> a2) -> IO ()
printSolution parser parser2 file answer1 answer2 =
  do
    f <- parseFromFile parser file
    case f of
      Left err -> print err
      Right g -> do
        putStrLn "AOC1 Answer:"
        print $ answer1 g
    f2 <- parseFromFile parser2 file
    case f2 of
      Left err -> print err
      Right g -> do
        putStrLn "AOC2 Answer:"
        print $ answer2 g

numbersAsWords :: IntMap String
numbersAsWords = I.fromList $ Prelude.zip [0 ..] ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]

wordsAsNumbers :: HashMap String Int
wordsAsNumbers = M.fromList $ Prelude.zip ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"] [0 ..]

word2num :: String -> Int
word2num s = wordsAsNumbers M.! s

num2word :: Int -> String
num2word n = numbersAsWords I.! n

type Point = (Int, Int)

parseListToMap :: [[(Int, a)]] -> [(Point, a)]
parseListToMap = Prelude.concat . Prelude.zipWith (\i b -> (\x -> ((fst x, i), snd x)) <$> b) [0 ..]

parseLineMap :: Parser garbage -> Parser interesting -> Parser (IntMap interesting)
parseLineMap g i = do
  x <- parseLineList g i
  return $ I.fromList x

parseGrid :: Parser garbage -> Parser interesting -> Parser (HashMap Point interesting)
parseGrid g i = do
  line <- parseLineList g i `sepEndBy1` endOfLine
  return $ M.fromList $ parseListToMap line

parseLineVector :: Parser garbage -> Parser interesting -> Parser (Vector (Int, interesting))
parseLineVector garbage interesting = V.fromList <$> parseLineList garbage interesting

parseLineList :: Parser garbage -> Parser interesting -> Parser [(Int, interesting)]
parseLineList garbage interesting = do
  x <- many $ do
    val <- skipUntil garbage interesting
    case val of
      Nothing -> return Nothing
      Just v -> do
        position <- getPosition
        return $ Just (sourceColumn position - 1, v)
  return $ Maybe.catMaybes x

skipUntil :: Parser a -> Parser end -> Parser (Maybe end)
skipUntil p end = scan
  where
    scan = Just <$> end <|> do _ <- p; scan <|> return Nothing
