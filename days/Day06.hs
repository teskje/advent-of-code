module Main where

import Aoc (getRawInput)
import Data.List (findIndex, tails)
import Data.Maybe (fromJust)
import qualified Data.Set as S

main :: IO ()
main = do
  input <- getRawInput 6
  print $ findMarkerEnd 4 input
  print $ findMarkerEnd 14 input

findMarkerEnd :: Int -> String -> Int
findMarkerEnd n = (+ n) . findMarker n

findMarker :: Int -> String -> Int
findMarker n = fromJust . findIndex allDistinct . windows n

allDistinct :: String -> Bool
allDistinct s = length (S.fromList s) == length s

windows :: Int -> [a] -> [[a]]
windows n xs = takeWhile ((== n) . length) ws
  where
    ws = map (take n) (tails xs)
