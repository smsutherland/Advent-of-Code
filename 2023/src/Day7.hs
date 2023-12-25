module Day7 (day7) where

import Common
import Data.Bifunctor
import Data.List
import Data.Maybe
import Parse

type Input = [(Hand, Int)]

data Hand = Hand
  { hand :: String,
    counts :: [(Char, Int)],
    power :: Int
  }
  deriving (Show)

instance Eq Hand where
  (==) (Hand h1 _ _) (Hand h2 _ _) = sort h1 == sort h2

instance Ord Hand where
  compare (Hand h1 _ p1) (Hand h2 _ p2) =
    case compare p1 p2 of
      EQ -> compareS h1 h2
      x -> x

compareS :: String -> String -> Ordering
compareS (a : as) (b : bs) = case compareC a b of
  EQ -> compareS as bs
  x -> x
compareS [] [] = EQ
compareS _ _ = undefined

compareC :: Char -> Char -> Ordering
compareC a b = compare (elemIndex a ord) (elemIndex b ord)
  where
    ord = ['2' .. '9'] ++ "TJQKA"

data Hand2 = Hand2
  { hand2 :: String,
    counts2 :: [(Char, Int)],
    power2 :: Int
  }
  deriving (Show)

instance Eq Hand2 where
  (==) (Hand2 h1 _ _) (Hand2 h2 _ _) = sort h1 == sort h2

instance Ord Hand2 where
  compare (Hand2 h1 _ p1) (Hand2 h2 _ p2) =
    case compare p1 p2 of
      EQ -> compareS2 h1 h2
      x -> x

compareS2 :: String -> String -> Ordering
compareS2 (a : as) (b : bs) = case compareC2 a b of
  EQ -> compareS2 as bs
  x -> x
compareS2 [] [] = EQ
compareS2 _ _ = undefined

compareC2 :: Char -> Char -> Ordering
compareC2 a b = compare (elemIndex a ord) (elemIndex b ord)
  where
    ord = 'J' : ['2' .. '9'] ++ "TQKA"

oneToTwo :: Hand -> Hand2
oneToTwo (Hand h c _) = Hand2 h c (newPower c)

mkhand :: String -> Hand
mkhand inp = Hand inp theseCounts pow
  where
    cards = nub inp
    count c = length . filter (== c)
    theseCounts = sortOn snd $ map (\c -> (c, count c inp)) cards
    pow = findPower theseCounts

findPower :: [(Char, Int)] -> Int
findPower c = case map snd c of
  [1, 1, 1, 1, 1] -> 1 -- high card
  [1, 1, 1, 2] -> 2 -- pair
  [1, 2, 2] -> 3 -- two pair
  [1, 1, 3] -> 4 -- three of a kind
  [2, 3] -> 5 -- full house
  [1, 4] -> 6 -- four of a kind
  [5] -> 7 -- five of a kind
  _ -> undefined

newPower :: [(Char, Int)] -> Int
newPower c = case (map snd c, snd <$> find ((== 'J') . fst) c) of
  (_, Nothing) -> findPower c
  ([1, 1, 1, 1, 1], Just 1) -> 2
  ([1, 1, 1, 2], Just 1) -> 4
  ([1, 1, 1, 2], Just 2) -> 4
  ([1, 2, 2], Just 1) -> 5
  ([1, 2, 2], Just 2) -> 6
  ([1, 1, 3], Just 1) -> 6
  ([1, 1, 3], Just 3) -> 6
  ([2, 3], Just 2) -> 7
  ([2, 3], Just 3) -> 7
  ([1, 4], Just 1) -> 7
  ([1, 4], Just 4) -> 7
  ([5], Just 5) -> 7
  _ -> undefined

inputP :: Parser Input
inputP = sepBy ws (bothP handP (ws *> numberP))

handP :: Parser Hand
handP = mkhand <$> wordP

day7 :: Day
day7 input = (part inp, part inp2)
  where
    inp = fromJust $ parse inputP input
    inp2 = map (first oneToTwo) inp
    part :: (Ord a) => [(a, Int)] -> Int
    part = sum . flip (zipWith (\h i -> snd h * i)) [1 ..] . sort
