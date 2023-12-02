

import Data.Char
import System.IO
import Data.List
import GHC.Conc (numCapabilities)
import Data.Maybe
import qualified Data.Semigroup as Int
import GHC.Base
import qualified Data.Ord as Ordering

main :: IO ()
main = do
  filecontent <- readFile "./aoc1.txt"
  --   Print without the lettered numbers
  print "AOC 1: Without looking at the letters"
  print $ sum <$> Prelude.mapM getNumber (lines filecontent)
  print "AOC 2: While looking at the letters"
  print $ sum $ map getNumberSecond (lines filecontent)

getNumber s = do
  a <- firstNumber s
  b <- firstNumber (reverse s)
  return (10 * a + b)

firstNumber [] = Nothing
firstNumber (x : xs)
  | isDigit x = Just $ digitToInt x
  | otherwise = firstNumber xs

letteredNumbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]

findString search strNum = findIndex (isPrefixOf $ letteredNumbers !! strNum) (tails search)
findStringR search strNum = findIndex (isPrefixOf $ reverse $ letteredNumbers !! strNum) (tails $ reverse search)

tFindString search strNum = (strNum, findString search strNum)
tFindStringR search strNum = (strNum, findStringR search strNum)

findNumber [] _ = Nothing
findNumber (x:xs) num
    | intToDigit num == x = Just 0
    | otherwise = do
        fn <- findNumber xs num
        return (1 + fn)
findNumberR :: String -> Int -> Maybe Int
findNumberR = findNumber . reverse

tFindNumber s n = (n, findNumber s n)
tFindNumberR :: String -> Int -> (Int, Maybe Int)
tFindNumberR s n = (n, findNumberR s n)

minStrings str = tFindString str <$> [0..9]
minStringsR str = tFindStringR str <$> [0..9]
minNumber str = tFindNumber str <$> [0..9]
minNumberR str = tFindNumberR str <$> [0..9]

minStringValue str = minimumBy numberLess $ minStrings str
minStringValueR str = minimumBy numberLess $ minStringsR str
minNumValue str = minimumBy numberLess $ minNumber str
minNumValueR str = minimumBy numberLess $ minNumberR str
minOverallValue str =
  let msv = minStringValue str
      mnv = minNumValue str
  in case numberLess msv mnv of
    LT -> fst msv
    EQ -> fst msv
    GT -> fst mnv

minOverallValueR str =
  let msv = minStringValueR str
      mnv = minNumValueR str
  in case numberLess msv mnv of
    LT -> fst msv
    EQ -> fst msv
    GT -> fst mnv

getNumberSecond str = minOverallValue str * 10 + minOverallValueR str

numberLess ma mb =
  let ua = snd ma
      ub = snd mb
      a = fromMaybe 10000 ua
      b = fromMaybe 10000 ub
  in compareInt a b



