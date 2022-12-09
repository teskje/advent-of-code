module Main where

import Aoc (Parser, getParsedLines)
import Data.Char (digitToInt)
import Text.Megaparsec (many)
import Text.Megaparsec.Char (digitChar)

main :: IO ()
main = do
  input <- getParsedLines 8 treeLine
  print $ countVisible input
  print $ findBestScore input

treeLine :: Parser [Int]
treeLine = many (digitToInt <$> digitChar)

type Grid = [[Int]]

countVisible :: Grid -> Int
countVisible g = sum $ map (fromEnum . isVisible g) (positions g)

isVisible :: Grid -> (Int, Int) -> Bool
isVisible g p = any (all smaller) (directions g p)
  where
    smaller p' = tree g p' < tree g p

findBestScore :: Grid -> Int
findBestScore g = maximum $ map (score g) (positions g)

score :: Grid -> (Int, Int) -> Int
score g p = product $ map viewDistance (directions g p)
  where
    viewDistance = length . takeWhileInclusive smaller
    smaller p' = tree g p' < tree g p

maxX :: Grid -> Int
maxX = subtract 1 . length . head

maxY :: Grid -> Int
maxY = subtract 1 . length

positions :: Grid -> [(Int, Int)]
positions g = [(x, y) | x <- [0 .. maxX g], y <- [0 .. maxY g]]

directions :: Grid -> (Int, Int) -> [[(Int, Int)]]
directions g p = [leftFrom g p, rightFrom g p, upFrom g p, downFrom g p]

leftFrom :: Grid -> (Int, Int) -> [(Int, Int)]
leftFrom _ (x, y) = reverse [(i, y) | i <- [0 .. x - 1]]

rightFrom :: Grid -> (Int, Int) -> [(Int, Int)]
rightFrom g (x, y) = [(i, y) | i <- [x + 1 .. maxX g]]

upFrom :: Grid -> (Int, Int) -> [(Int, Int)]
upFrom _ (x, y) = reverse [(x, i) | i <- [0 .. y - 1]]

downFrom :: Grid -> (Int, Int) -> [(Int, Int)]
downFrom g (x, y) = [(x, i) | i <- [y + 1 .. maxY g]]

tree :: Grid -> (Int, Int) -> Int
tree grid (x, y) = (grid !! y) !! x

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs)
  | p x = x : takeWhileInclusive p xs
  | otherwise = [x]
