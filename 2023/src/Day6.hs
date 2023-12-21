module Day6 (day6) where

import Common
import Control.Applicative
import Data.Char
import Data.Maybe
import Parse

type Input = [(Int, Int)]

inputP :: Parser Input
inputP =
  zip
    <$> (stringP "Time:" *> ws *> sepBy ws numberP)
    <*> (stringP "\nDistance:" *> ws *> sepBy ws numberP)

number2P :: Parser Int
number2P = read . filter isDigit <$> some (parseIf (\c -> isDigit c || isSpace c))

input2P :: Parser (Int, Int)
input2P =
  (,)
    <$> (stringP "Time:" *> ws *> number2P)
    <*> (stringP "Distance:" *> ws *> number2P)

numWays :: (Int, Int) -> Int
numWays (b, c) =
  if even b
    then 2 * ceiling (det - 1) + 1
    else 2 * ceiling (det - 0.5)
  where
    det :: Double
    det = sqrt (fromIntegral (b * b - 4 * c)) / 2

day6 :: Day
day6 input = (part1, part2)
  where
    inp = fromJust $ parse inputP input
    inp2 = fromJust $ parse input2P input
    part1 = product $ map numWays inp
    part2 = numWays inp2
