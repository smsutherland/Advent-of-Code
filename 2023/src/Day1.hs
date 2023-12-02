module Day1 (day1) where

import Common
import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe

digits :: [String]
digits =
  [ "zero",
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

firstDigit1 :: String -> Int
firstDigit1 = maybe 0 digitToInt . find isDigit

lastDigit1 :: String -> Int
lastDigit1 = digitToInt . foldr1 (\_ a -> a) . filter isDigit

startsWithTextDigit :: String -> Maybe Int
startsWithTextDigit = flip findIndex digits . flip isPrefixOf

startsWithNumDigit :: String -> Maybe Int
startsWithNumDigit = flip elemIndex ['0' .. '9'] . head

startsWithDigit :: String -> Maybe Int
startsWithDigit = (<|>) <$> startsWithNumDigit <*> startsWithTextDigit

firstDigit2 :: String -> Int
firstDigit2 = liftA2 fromMaybe (firstDigit2 . tail) startsWithDigit

findLastDigit :: String -> Maybe Int
findLastDigit [] = Nothing
findLastDigit x = findLastDigit (tail x) <|> startsWithDigit x

lastDigit2 :: String -> Int
lastDigit2 = fromMaybe 0 . findLastDigit

day1 :: Day
day1 input = (part1 l, part2 l)
  where
    l = splitLines input
    part1 = liftA2 (+) ((10 *) . (sum . fmap firstDigit1)) $ sum . fmap lastDigit1
    part2 = liftA2 (+) ((10 *) . (sum . fmap firstDigit2)) $ sum . fmap lastDigit2
