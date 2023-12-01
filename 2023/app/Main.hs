module Main where

import qualified Aoc (runDay)

main :: IO ()
main = do
  putStrLn "Enter day number: "
  day <- getLine >>= readIO :: IO Int
  Aoc.runDay day >>= print
