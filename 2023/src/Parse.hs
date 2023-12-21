module Parse where

import Control.Applicative
import Data.Char
import Data.List

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

lineP :: Parser String
lineP = spanP (/= '\n') <* charP '\n'

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
stringLitP = some charLitP

charLitP :: Parser Char
charLitP = parseIf (`elem` ['a' .. 'z'])

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

bothP :: Parser a -> Parser b -> Parser (a, b)
bothP p1 p2 = (,) <$> p1 <*> p2
