{-# LANGUAGE TupleSections #-}

module Day4 (day4) where

import Common
import Data.List
import Data.Maybe
import Parse

data Card = Card
  { gameNum :: Int,
    win :: [Int],
    nums :: [Int]
  }
  deriving (Show, Eq)

cardP :: Parser Card
cardP =
  Card
    <$> (stringP "Card" *> ws *> numberP <* charP ':' <* ws)
    <*> (sepBy ws numberP <* ws <* charP '|' <* ws)
    <*> sepBy ws numberP

numWins :: Card -> Int
numWins c = length (win c `intersect` nums c)

countCards :: [(Int, Card)] -> Int
countCards [] = 0
countCards (c : cs) = fst c + countCards (addCards cs (fst c) (numWins $ snd c))

--          cards            times  count
addCards :: [(Int, Card)] -> Int -> Int -> [(Int, Card)]
addCards cards _ 0 = cards
addCards (c : cs) t n = (fst c + t, snd c) : addCards cs t (n - 1)
addCards [] _ _ = []

day4 :: Day
day4 input = (part1, part2)
  where
    cards = snd . fromJust $ runParser (sepBy ws cardP) input
    part1 = sum $ map (\c -> quot (2 ^ numWins c) 2) cards
    part2 = countCards $ map (1,) cards
