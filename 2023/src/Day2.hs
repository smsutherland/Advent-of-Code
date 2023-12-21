module Day2 (day2) where

import Common
import Data.List
import Parse

data Game = Game
  { num :: Int,
    rounds :: [Round]
  }
  deriving (Show, Eq)

newtype Round = Round
  { colors :: [(Int, String)]
  }
  deriving (Show, Eq)

gameP :: Parser Game
gameP =
  Game
    <$> (stringP "Game" *> ws *> numberP)
    <*> (charP ':' *> ws *> sepBy (charP ';' *> ws) roundP)

roundP :: Parser Round
roundP =
  Round
    <$> sepBy (charP ',' *> ws) (bothP numberP (ws *> stringLitP))

inputP :: Parser [Game]
inputP = sepBy (charP '\n') gameP

data GameH = GameH
  { numH :: Int,
    roundsH :: [RoundH]
  }

data RoundH = RoundH
  { red :: Int,
    green :: Int,
    blue :: Int
  }

handleGame :: Game -> GameH
handleGame game = do
  let idH = num game
  GameH idH $ map handleRound $ rounds game

handleRound :: Round -> RoundH
handleRound r = do
  let redH = maybe 0 fst $ find (("red" ==) . snd) (colors r)
  let greenH = maybe 0 fst $ find (("green" ==) . snd) (colors r)
  let blueH = maybe 0 fst $ find (("blue" ==) . snd) (colors r)
  RoundH redH greenH blueH

maxRound :: RoundH -> RoundH -> RoundH
maxRound x y = RoundH (max (red x) (red y)) (max (green x) (green y)) (max (blue x) (blue y))

maxRounds :: [RoundH] -> RoundH
maxRounds = foldr maxRound (RoundH 0 0 0)

possibleGame :: GameH -> Bool
possibleGame g = do
  let maxes = maxRounds $ roundsH g
  (12 >= red maxes) && (13 >= green maxes) && (14 >= blue maxes)

day2 :: Day
day2 input = (part1 l, part2 l)
  where
    l = map handleGame . maybe undefined snd $ runParser inputP input
    part1 = sum . map numH . filter possibleGame
    part2 = sum . map ((\x -> red x * green x * blue x) . maxRounds . roundsH)
