module Day2 (day2) where

import Common
import Control.Applicative
import Data.Char
import Data.List

data Game = Game
  { num :: Int,
    rounds :: [Round]
  }
  deriving (Show, Eq)

newtype Round = Round
  { colors :: [(Int, String)]
  }
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f input = case uncons input of
      Just (y, ys)
        | y == x -> Just (ys, x)
        | otherwise -> Nothing
      _ -> Nothing

stringP :: String -> Parser String
stringP str =
  Parser $ \input ->
    runParser (traverse charP str) input

spanP :: (Char -> Bool) -> Parser String
spanP = many . parseIf

parseIf :: (Char -> Bool) -> Parser Char
parseIf f = Parser $ \input ->
  case uncons input of
    Just (y, ys)
      | f y -> Just (ys, y)
    _ -> Nothing

ws :: Parser String
ws = spanP isSpace

numberP :: Parser Int
numberP = read <$> some (parseIf isDigit)

stringLitP :: Parser String
stringLitP = many charLitP

charLitP :: Parser Char
charLitP = parseIf (`elem` ['a' .. 'z'])

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

lineP :: Parser Game
lineP =
  Game
    <$> (stringP "Game" *> ws *> numberP)
    <*> (charP ':' *> ws *> sepBy (charP ';' *> ws) roundP)

bothP :: Parser a -> Parser b -> Parser (a, b)
bothP p1 p2 = (,) <$> p1 <*> p2

roundP :: Parser Round
roundP =
  Round
    <$> sepBy (charP ',' *> ws) (bothP numberP (ws *> stringLitP))

inputP :: Parser [Game]
inputP = sepBy (charP '\n') lineP

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
