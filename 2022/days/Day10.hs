module Main where

import Aoc (Parser, getParsedLines, numberP)
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Char (string)
import Text.Printf (printf)

main :: IO ()
main = do
  input <- getParsedLines 10 instruction
  let states = execute input
  print $ signalStrength states
  printf "%s" (renderPixels states)

data Inst = Addx Int | Noop
  deriving (Show)

instruction :: Parser Inst
instruction = addx <|> noop
  where
    addx = Addx <$ string "addx " <*> numberP
    noop = Noop <$ string "noop"

execute :: [Inst] -> [Int]
execute = execute' 1

execute' :: Int -> [Inst] -> [Int]
execute' x [] = [x]
execute' x (Addx n : is) = x : x : execute' (x + n) is
execute' x (Noop : is) = x : execute' x is

signalStrength :: [Int] -> Int
signalStrength = sum . map (uncurry (*)) . takeCycles
  where
    cycles = [20, 60 .. 220]
    takeCycles = filter (\(c, _) -> c `elem` cycles) . zip [1 ..]

renderPixels :: [Int] -> String
renderPixels = unlines . map renderRow . chunksOf 40
  where
    getPixel n x
      | n >= x && n <= x + 2 = '█'
      | otherwise = ' '
    renderRow = zipWith getPixel [1 ..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = chunk : chunksOf n rest
  where
    (chunk, rest) = splitAt n xs
