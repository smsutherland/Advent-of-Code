module Day5 (day5) where

import Common
import Control.Applicative
import Data.Maybe
import Parse

data MapPart = MapPart
  { dest :: Int,
    src :: Int,
    len :: Int
  }
  deriving (Show)

newtype Map = Map
  { parts :: [MapPart]
  }
  deriving (Show)

data Input = Input
  { seeds :: [Int],
    maps :: [Map]
  }
  deriving (Show)

data Input2 = Input2
  { seeds2 :: [(Int, Int)],
    maps2 :: [Map]
  }

oneToTwo :: Input -> Input2
oneToTwo i = Input2 (zip (first $ seeds i) (second $ seeds i)) (maps i)

first :: [a] -> [a]
first [] = []
first (x : xs) = x : second xs

second :: [a] -> [a]
second [] = []
second (_ : xs) = first xs

mapPartP :: Parser MapPart
mapPartP = MapPart <$> numberP <* ws <*> numberP <* ws <*> numberP

mapP :: Parser Map
mapP = Map <$> (ws *> lineP *> sepBy ws mapPartP)

inputP :: Parser Input
inputP =
  Input
    <$> (stringP "seeds: " *> sepBy ws numberP)
    <*> many mapP

applyMapPart :: MapPart -> Int -> Maybe Int
applyMapPart part i = if src part <= i && i - src part < len part then Just $ dest part + i - src part else Nothing

applyMap :: Map -> Int -> Int
applyMap m i = fromMaybe i $ foldl (<|>) Nothing $ map (`applyMapPart` i) (parts m)

end :: (Int, Int) -> Int
end (a, b) = a + b

intersection :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
intersection a b
  | a > b = intersection b a
  | end a > fst b =
      if end a <= end b
        then Just (fst b, end a - fst b)
        else Just b
  | otherwise = Nothing

subtractRange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
subtractRange a b
  | a == b = []
  | fst a == fst b = [(end b, snd a - snd b)]
  | end a == end b = [(fst a, fst b - fst a)]
  | otherwise = [(fst a, fst b - fst a), (end b, end a - end b)]

--                                         new               old
applyMapPart2 :: MapPart -> (Int, Int) -> (Maybe (Int, Int), [(Int, Int)])
applyMapPart2 part i =
  maybe
    (Nothing, [i])
    (\inter2 -> (Just (dest part + fst inter2 - src part, snd inter2), subtractRange i inter2))
    (intersection (src part, len part) i)

--                                              new           old
applyMapPartMany :: MapPart -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
applyMapPartMany _ [] = ([], [])
applyMapPartMany m (i : is) = (maybeToList new ++ new2, old ++ old2)
  where
    (new, old) = applyMapPart2 m i
    (new2, old2) = applyMapPartMany m is

applyMap2 :: [MapPart] -> [(Int, Int)] -> [(Int, Int)]
applyMap2 [] i = i
applyMap2 (m : ms) i = new ++ applyMap2 ms old
  where
    (new, old) = applyMapPartMany m i

day5 :: Day
day5 input = (part1, part2)
  where
    inp = snd . fromJust $ runParser inputP input
    inp2 = oneToTwo inp
    part1 = minimum $ foldl (\s m -> map (applyMap m) s) (seeds inp) (maps inp)
    part2 = fst . minimum $ foldl (flip applyMap2) (seeds2 inp2) (map parts $ maps2 inp2)
