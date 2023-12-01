module Day1 (day1) where

import Common
import Data.Char
import Data.Maybe

digits :: [String]
digits = [
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine"
    ]

firstDigit :: String -> Int
firstDigit = fromMaybe 0 . fmap digitToInt . listToMaybe . filter isDigit

lastDigit :: String -> Int
lastDigit = digitToInt . foldr1 (\_ a -> a) . filter isDigit

day1 :: Day
day1 input = (part1 l, part2 input)
    where
    l = splitLines input
    part1 = liftA2 (+) ((10 *) . (foldr (+) 0 . fmap firstDigit)) $ foldr (+) 0 . fmap lastDigit
    part2 _ = 0
