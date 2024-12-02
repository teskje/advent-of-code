module Main where

import Aoc (Parser, getParsedLines)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (decimal)

main :: IO ()
main = do
  reports <- getParsedLines 2 report
  print $ (length . filter safe) reports
  print $ (length . filter relaxedSafe) reports

report :: Parser [Int]
report = decimal `sepBy` space

safe :: [Int] -> Bool
safe ls = increasing forward || increasing backward
  where
    forward = zip ls (tail ls)
    backward = zip (tail ls) ls
    increasing = all $ \(a, b) -> (a - b) `elem` [1 .. 3]

relaxedSafe :: [Int] -> Bool
relaxedSafe ls = any safe dampened
  where
    dampened = [dampen i | i <- [0 .. length ls - 1]]
    dampen i = take i ls ++ drop (i + 1) ls
