module Main where

import Aoc (getParsedLines, numberP)
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  input1 <- getParsedLines 20 numberP
  print $ (sum . coordinates . mix) input1

  let input2 = map (* 811589153) input1
  print $ (sum . coordinates . mix10) input2

mix :: [Int] -> [Int]
mix = map snd . mix' 0 . zip [0 ..]

mix10 :: [Int] -> [Int]
mix10 = map snd . (!! 10) . iterate (mix' 0) . zip [0 ..]

mix' :: Int -> [(Int, Int)] -> [(Int, Int)]
mix' r xis = case findIndex ((== r) . fst) xis of
  Just i -> mix' (r + 1) (move i xis)
  Nothing -> xis

move :: Int -> [(Int, Int)] -> [(Int, Int)]
move i xis = insertAt j' xi xis'
  where
    (xi, xis') = popAt i xis
    j = (i + snd xi) `mod` length xis'
    j' = if j == 0 then length xis' else j

popAt :: Int -> [a] -> (a, [a])
popAt i xs = (x, xs')
  where
    x = xs !! i
    xs' = take i xs ++ drop (i + 1) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = take i xs ++ [x] ++ drop i xs

coordinates :: [Int] -> [Int]
coordinates xs = map coord [1000, 2000, 3000]
  where
    i0 = fromJust $ elemIndex 0 xs
    coord i = xs !! ((i + i0) `mod` length xs)
