module Main where

import Aoc (getInputLines)
import Control.Monad.State (State, evalState, get, put)
import Data.Char (ord)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  grid <- getInputLines 12
  let start = gridFind grid 'E'
  let path = findPath grid start 'S'
  let scenicPath = findPath grid start 'a'
  print $ countSteps path
  print $ countSteps scenicPath

type Pos = (Int, Int)

type Grid = [[Char]]

data Search = Search
  { sGrid :: Grid,
    sUnvisited :: Set Pos
  }

gridPositions :: Grid -> [Pos]
gridPositions grid = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
  where
    w = length (head grid)
    h = length grid

gridGet :: Grid -> Pos -> Char
gridGet grid (x, y) = (grid !! y) !! x

gridFind :: Grid -> Char -> Pos
gridFind grid c = fromJust $ find check (gridPositions grid)
  where
    check p = gridGet grid p == c

findPath :: Grid -> Pos -> Char -> [Pos]
findPath grid start c = evalState (search c [[start]]) state
  where
    unvisited = (S.delete start . S.fromList . gridPositions) grid
    state = Search {sGrid = grid, sUnvisited = unvisited}

search :: Char -> [[Pos]] -> State Search [Pos]
search c pts = do
  state <- get
  let grid = sGrid state
  case find (\pt -> gridGet grid (last pt) == c) pts of
    Just pt -> return pt
    Nothing -> step pts >>= search c

step :: [[Pos]] -> State Search [[Pos]]
step [] = return []
step (pt : pts) = do
  pts' <- step1 pt
  pts'' <- step pts
  return $ pts' ++ pts''

step1 :: [Pos] -> State Search [[Pos]]
step1 pt = do
  nexts <- visitNexts (last pt)
  return [pt ++ [p] | p <- nexts]

visitNexts :: Pos -> State Search [Pos]
visitNexts p = do
  state <- get
  let grid = sGrid state
  let unvisited = sUnvisited state
  let candidates = filter (`S.member` unvisited) (neighbors p)
  let nexts = filter (\p' -> gridGet grid p `reaches` gridGet grid p') candidates
  let unvisited' = S.filter (`notElem` nexts) unvisited
  put state {sUnvisited = unvisited'}
  return nexts

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

reaches :: Char -> Char -> Bool
reaches a b = (ord a' - ord b') <= 1
  where
    a' = if a == 'E' then 'a' else a
    b' = if b == 'S' then 'z' else b

countSteps :: [Pos] -> Int
countSteps pt = length pt - 1
