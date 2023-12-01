module Common (Day, splitGroups, splitLines) where

import Data.List.Split (split, onSublist, dropDelims, dropBlanks)

type Day = String -> (Int, Int)

splitGroups :: String -> [String]
splitGroups input = split (dropDelims . dropBlanks $ onSublist "\n\n") input

splitLines :: String -> [String]
splitLines = split (dropDelims . dropBlanks $ onSublist "\n")
