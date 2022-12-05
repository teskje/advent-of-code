module Main where

import Aoc (Parser, getParsedInput)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Text.Megaparsec (between, many, manyTill, sepBy, try, (<|>))
import Text.Megaparsec.Char (char, newline, string, upperChar)
import Text.Megaparsec.Char.Lexer (decimal)

main :: IO ()
main = do
  input <- getParsedInput 5 puzzleInput
  print $ map head (uncurry applySteps1 input)
  print $ map head (uncurry applySteps2 input)

type Stacks = [String]

type Step = (Int, Int, Int)

puzzleInput :: Parser (Stacks, [Step])
puzzleInput = do
  crates <- manyTill crateLine (try crateNumbers)
  _ <- newline
  steps <- many stepLine
  let stacks = map catMaybes (transpose crates)
  return (stacks, steps)

crateLine :: Parser [Maybe Char]
crateLine = maybeCrate `sepBy` char ' ' <* newline

maybeCrate :: Parser (Maybe Char)
maybeCrate = (Just <$> crate) <|> (Nothing <$ empty)
  where
    crate = between (char '[') (char ']') upperChar
    empty = string "   "

crateNumbers :: Parser [Int]
crateNumbers = number `sepBy` char ' ' <* newline
  where
    number = between (char ' ') (char ' ') decimal

stepLine :: Parser Step
stepLine = do
  _ <- string "move "
  move <- decimal
  _ <- string " from "
  from <- decimal
  _ <- string " to "
  to <- decimal
  _ <- newline
  return (move, from, to)

applySteps1 :: Stacks -> [Step] -> Stacks
applySteps1 = foldl (applyStep reverse)

applySteps2 :: Stacks -> [Step] -> Stacks
applySteps2 = foldl (applyStep id)

type OrderFn = String -> String

applyStep :: OrderFn -> Stacks -> Step -> Stacks
applyStep order stacks (move, from, to) = zipWith apply [1 ..] stacks
  where
    apply i
      | i == from = drop move
      | i == to = (order moved ++)
      | otherwise = id
    moved = take move (stacks !! (from - 1))
