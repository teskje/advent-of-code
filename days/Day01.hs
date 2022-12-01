module Main where

import Aoc (Parser, getParsedInput)
import Data.List (sortBy)
import Text.Megaparsec (many, sepBy)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

main :: IO ()
main = do
  calories <- getParsedInput 1 caloriesList
  let totals = map sum calories
  let sorted = sortBy (flip compare) totals
  print $ head sorted
  print $ sum (take 3 sorted)

caloriesList :: Parser [[Int]]
caloriesList = sepBy (many calories) newline
  where calories = decimal <* newline
