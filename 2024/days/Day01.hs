{-# LANGUAGE TupleSections #-}

module Main where

import Aoc (Parser, getParsedLines)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (decimal)

main :: IO ()
main = do
  (ls, rs) <- unzip <$> getParsedLines 1 idPair
  print $ totalDistance ls rs
  print $ similarityScore ls rs

idPair :: Parser (Int, Int)
idPair = (,) <$> decimal <* space <*> decimal

totalDistance :: [Int] -> [Int] -> Int
totalDistance ls rs = sum $ map diff pairs
  where
    pairs = zip (sort ls) (sort rs)
    diff (l, r) = abs (l - r)

similarityScore :: [Int] -> [Int] -> Int
similarityScore ls rs = sum $ map score ls
  where
    counts = M.fromListWith (+) $ map (,1) rs
    score x = x * M.findWithDefault 0 x counts
