module Main where

import Aoc (getInputLines)
import Data.Char (isLower, ord)
import Data.List (intersect)

main :: IO ()
main = do
  input <- getInputLines 3
  print $ (prioritize . findDuplicates . splitRucksacks) input
  print $ (prioritize . findDuplicates . splitGroups) input

splitRucksacks :: [String] -> [[String]]
splitRucksacks = map (listify2 . halves)

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

listify2 :: (a, a) -> [a]
listify2 (a, b) = [a, b]

splitGroups :: [String] -> [[String]]
splitGroups [] = []
splitGroups ss = group : splitGroups rest
  where
    (group, rest) = splitAt 3 ss

findDuplicates :: [[String]] -> String
findDuplicates = map findDuplicate

findDuplicate :: [String] -> Char
findDuplicate = head . foldl1 intersect

prioritize :: String -> Int
prioritize = sum . map priority

priority :: Char -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27
