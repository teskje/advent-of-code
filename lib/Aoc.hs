module Aoc where

import Control.Arrow (left)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)
import Text.Printf (printf)

type Parser = Parsec Void String

getParsedInput :: Int -> Parser a -> IO a
getParsedInput day p = do
  input <- getRawInput day
  either fail return (parseString p input)

getParsedLines :: Int -> Parser a -> IO [a]
getParsedLines day p = do
  input <- getRawInput day
  either fail return (parseLines p input)

getRawInput :: Int -> IO String
getRawInput = readFile . inputFileName

inputFileName :: Int -> String
inputFileName = printf "inputs/day%02d.txt"

parseLines :: Parser a -> String -> Either String [a]
parseLines p s = traverse (parseString p) (lines s)

parseString :: Parser a -> String -> Either String a
parseString p s = left errorBundlePretty (runParser p "" s)
