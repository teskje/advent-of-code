module Main where

import Aoc (Parser, getParsedLines, numberP)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, string)

main :: IO ()
main = do
  input <- getParsedLines 14 rockPath
  let grid = plotPaths input
  print $ runSand grid

  let grid' = makeFloored grid
  print $ runSand grid'

type Pos = (Int, Int)

data Grid = Floorless (Map Pos ()) | Floored (Map Pos ()) Int
  deriving (Show, Eq)

gridSet :: Pos -> Grid -> Grid
gridSet pos (Floorless g) = Floorless (M.insert pos () g)
gridSet pos (Floored g yb) = Floored (M.insert pos () g) yb

gridFree :: Pos -> Grid -> Bool
gridFree pos (Floorless g) = M.notMember pos g
gridFree (x, y) (Floored g yb)
  | y == yb = False
  | otherwise = M.notMember (x, y) g

makeFloored :: Grid -> Grid
makeFloored (Floorless g) = Floored g (maxY g + 2)
  where
    maxY = maximum . map snd . M.keys
makeFloored (Floored _ _) = error "already floored"

rockPath :: Parser [Pos]
rockPath = sepBy positionP (string " -> ")

positionP :: Parser Pos
positionP = do
  x <- numberP
  _ <- char ','
  y <- numberP
  return (x, y)

plotPaths :: [[Pos]] -> Grid
plotPaths = foldl plotPath (Floorless M.empty)

plotPath :: Grid -> [Pos] -> Grid
plotPath grid path = foldl plotLine grid (pairs path)

pairs :: [a] -> [(a, a)]
pairs (a : b : xs) = (a, b) : pairs (b : xs)
pairs _ = []

plotLine :: Grid -> (Pos, Pos) -> Grid
plotLine grid (start, end) = foldl plot grid (line start end)
  where
    plot g pos = gridSet pos g

line :: Pos -> Pos -> [Pos]
line p1 p2
  | x1 == x2 = [(x1, y) | y <- [y1 .. y2]]
  | y1 == y2 = [(x, y1) | x <- [x1 .. x2]]
  | otherwise = error "not a straight line"
  where
    (x1, y1) = min p1 p2
    (x2, y2) = max p1 p2

runSand :: Grid -> Int
runSand grid =
  let grid' = stepSand grid
   in if grid' == grid
        then 0
        else 1 + runSand grid'

stepSand :: Grid -> Grid
stepSand grid = case dropSand grid (500, 0) of
  Just pos -> gridSet pos grid
  Nothing -> grid

dropSand :: Grid -> Pos -> Maybe Pos
dropSand grid (x, y)
  | y > 1000 = Nothing
  | free (x, y') = dropSand grid (x, y')
  | free (x - 1, y') = dropSand grid (x - 1, y')
  | free (x + 1, y') = dropSand grid (x + 1, y')
  | otherwise = Just (x, y)
  where
    y' = y + 1
    free pos = gridFree pos grid
