module Common (Day, splitGroups, splitLines) where

import Data.List.Split (dropBlanks, dropDelims, onSublist, split)

type Day = String -> (Int, Int)

splitGroups :: String -> [String]
splitGroups = split $ dropDelims . dropBlanks $ onSublist "\n\n"

splitLines :: String -> [String]
splitLines = split $ dropDelims . dropBlanks $ onSublist "\n"
