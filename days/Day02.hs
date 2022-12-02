module Main where

import Aoc (Parser, getParsedLines)
import Data.Functor (($>))
import Text.Megaparsec (choice)
import Text.Megaparsec.Char (char, space)

main :: IO ()
main = do
  input <- getParsedLines 2 inputLine
  print $ sum (map score1 input)
  print $ sum (map score2 input)

data X = A | B | C

inputLine :: Parser (X, X)
inputLine = do
  x1 <- choice [char 'A' $> A, char 'B' $> B, char 'C' $> C]
  space
  x2 <- choice [char 'X' $> A, char 'Y' $> B, char 'Z' $> C]
  return (x1, x2)

score1 :: (X, X) -> Int
score1 (x1, x2) = shapeScore s2 + outcomeScore (outcome s1 s2)
  where
    s1 = xToShape x1
    s2 = xToShape x2

score2 :: (X, X) -> Int
score2 (x1, x2) = shapeScore s2 + outcomeScore oc
  where
    s1 = xToShape x1
    oc = xToOutcome x2
    s2 = case (s1, oc) of
      (Rock, Loss) -> Scissors
      (Rock, Win) -> Paper
      (Paper, Loss) -> Rock
      (Paper, Win) -> Scissors
      (Scissors, Loss) -> Paper
      (Scissors, Win) -> Rock
      (s, Draw) -> s

data Shape = Rock | Paper | Scissors

xToShape :: X -> Shape
xToShape A = Rock
xToShape B = Paper
xToShape C = Scissors

data Outcome = Loss | Draw | Win

xToOutcome :: X -> Outcome
xToOutcome A = Loss
xToOutcome B = Draw
xToOutcome C = Win

outcome :: Shape -> Shape -> Outcome
outcome Rock Paper = Win
outcome Rock Scissors = Loss
outcome Paper Rock = Loss
outcome Paper Scissors = Win
outcome Scissors Rock = Win
outcome Scissors Paper = Loss
outcome _ _ = Draw

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

outcomeScore :: Outcome -> Int
outcomeScore Loss = 0
outcomeScore Draw = 3
outcomeScore Win = 6
