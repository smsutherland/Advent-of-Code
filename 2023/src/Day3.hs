module Day3 (day3) where

import Common
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

getDimensions :: [String] -> (Int, Int)
getDimensions input = (height input, width input)
  where
    height = length
    width = length . head

data Number = Number
  { value :: Int,
    position :: (Int, Int), -- x, y
    len :: Int
  }
  deriving (Show, Eq)

findNumbers :: [String] -> [Number]
findNumbers = concatMap (uncurry findNumbersInLine) . zip [0 ..]

findNumbersInLine :: Int -> String -> [Number]
findNumbersInLine lineNum = map (makeNumber lineNum) . filter (not . null) . splitWhen (not . isDigit . snd) . zip [0 ..]

makeNumber :: Int -> [(Int, Char)] -> Number
makeNumber lineNum parts = Number (read $ map snd parts) (fst $ head parts, lineNum) (length parts)

findSymbols :: [String] -> [(Int, Int)]
findSymbols l = concatMap (uncurry findSymbolsInLine) (zip [0 ..] l)

findSymbolsInLine :: Int -> String -> [(Int, Int)]
findSymbolsInLine lineNum = map (\x -> (fst x, lineNum)) . filter (isSymboll . snd) . zip [0 ..]

isSymboll :: Char -> Bool
isSymboll = not . (`elem` ['0' .. '9'] ++ ['.'])

findGears :: [String] -> [(Int, Int)]
findGears l = concatMap (uncurry findGearsInLine) (zip [0 ..] l)

findGearsInLine :: Int -> String -> [(Int, Int)]
findGearsInLine lineNum = map (\x -> (fst x, lineNum)) . filter ((== '*') . snd) . zip [0 ..]

--                 Point of concern
--                               dimensions
adjacentIndices :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
adjacentIndices x d = filter (`inDimension` d) $ map (pointSum x) offsets
  where
    offsets = filter (/= (0, 0)) [(a, b) | a <- [-1 .. 1], b <- [-1 .. 1]]

inDimension :: (Int, Int) -> (Int, Int) -> Bool
inDimension x d = both (>= 0) x && both (> 0) (pointDiff d x)

both :: (a -> Bool) -> (a, a) -> Bool
both f (x, y) = f x && f y

pointSum :: (Int, Int) -> (Int, Int) -> (Int, Int)
pointSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pointDiff :: (Int, Int) -> (Int, Int) -> (Int, Int)
pointDiff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

containsPoint :: Number -> (Int, Int) -> Bool
containsPoint n p = snd p == snd (position n) && fst p >= fst (position n) && fst p < fst (position n) + len n

day3 :: Day
day3 input = (part1, part2)
  where
    l = splitLines input
    dimensions = getDimensions l
    numbers = findNumbers l
    symbols = findSymbols l
    gears = findGears l
    part1 = sum $ map value $ nub $ mapMaybe (\p -> find (`containsPoint` p) numbers) $ nub $ concatMap (`adjacentIndices` dimensions) symbols
    part2 = sum $ map (product . map value) $ filter (\x -> length x == 2) $ map (nub . \x -> mapMaybe (\p -> find (`containsPoint` p) numbers) $ adjacentIndices x dimensions) gears
