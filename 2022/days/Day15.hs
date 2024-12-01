module Main where

import Aoc (Parser, getParsedLines, numberP)
import Aoc.RangeSet (RSet)
import qualified Aoc.RangeSet as RS
import Text.Megaparsec.Char (string)

main :: IO ()
main = do
  input <- getParsedLines 15 sensorLine
  let occupied = getOccupied input
  let reaches = getReaches input

  let y = 2000000
  let bounds = getXBounds reaches
  print $ RS.length (freeInRow y bounds occupied reaches)

  let bounds' = (0, 4000000)
  let dark = head (findDark bounds' occupied reaches)
  print $ tuningFrequency dark

type Pos = (Int, Int)

sensorLine :: Parser (Pos, Pos)
sensorLine = do
  _ <- string "Sensor at "
  sp <- positionP
  _ <- string ": closest beacon is at "
  bp <- positionP
  return (sp, bp)

positionP :: Parser Pos
positionP = do
  x <- string "x=" *> numberP
  _ <- string ", "
  y <- string "y=" *> numberP
  return (x, y)

getOccupied :: [(Pos, Pos)] -> [Pos]
getOccupied = concatMap (\(s, b) -> [s, b])

getReaches :: [(Pos, Pos)] -> [(Pos, Int)]
getReaches = map (\(s, b) -> (s, distance s b))

distance :: Pos -> Pos -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getXBounds :: [(Pos, Int)] -> (Int, Int)
getXBounds = foldl update (0, 0)
  where
    update (mi, ma) ((x, _), d) = (min mi (x - d), max ma (x + d))

darkInRow :: Int -> (Int, Int) -> [Pos] -> [(Pos, Int)] -> RSet
darkInRow y bounds occupied reaches = row''
  where
    row = RS.fromRange bounds
    yOccupied = [xi | (xi, yi) <- occupied, yi == y]
    yReaches = map (projectReach y) reaches
    row' = foldl (flip RS.delete) row yReaches
    row'' = foldl (flip RS.delete1) row' yOccupied

projectReach :: Int -> (Pos, Int) -> (Int, Int)
projectReach y (s, d)
  | distance s (xs, y) <= d = (start, end)
  | otherwise = (1, 0)
  where
    (xs, ys) = s
    dy = abs (y - ys)
    start = xs - d + dy
    end = xs + d - dy

freeInRow :: Int -> (Int, Int) -> [Pos] -> [(Pos, Int)] -> RSet
freeInRow y bounds occupied reaches = free
  where
    row = RS.fromRange bounds
    yOccupied = [xi | (xi, yi) <- occupied, yi == y]
    dark = darkInRow y bounds occupied reaches
    light = RS.foldl (flip RS.delete) row dark
    free = foldl (flip RS.delete1) light yOccupied

findDark :: (Int, Int) -> [Pos] -> [(Pos, Int)] -> [Pos]
findDark (mi, ma) occupied reaches = concatMap find [mi .. ma]
  where
    dark y = darkInRow y (mi, ma) occupied reaches
    find y = (map (\x -> (x, y)) . RS.explode . dark) y

tuningFrequency :: Pos -> Int
tuningFrequency (x, y) = x * 4000000 + y
