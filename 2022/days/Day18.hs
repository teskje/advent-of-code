module Main where

import Aoc (Parser, getParsedLines, numberP)
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec.Char (char)

main :: IO ()
main = do
  input <- getParsedLines 18 coordinate
  let cubes = S.fromList input
  print $ countSurface cubes
  print $ countOuterSurface cubes

type Coord = (Int, Int, Int)

coordinate :: Parser Coord
coordinate = do
  x <- numberP
  _ <- char ','
  y <- numberP
  _ <- char ','
  z <- numberP
  return (x, y, z)

countSurface :: Set Coord -> Int
countSurface cubes = sum $ map (countFreeSides cubes) (S.elems cubes)

countFreeSides :: Set Coord -> Coord -> Int
countFreeSides cubes co = length free
  where
    free = filter (`S.notMember` cubes) (neighbors co)

countOuterSurface :: Set Coord -> Int
countOuterSurface cubes = sum $ map (countOuterSides cubes) (S.elems cubes)

countOuterSides :: Set Coord -> Coord -> Int
countOuterSides cubes co = length outer
  where
    bounds = getBounds cubes
    free = filter (`S.notMember` cubes) (neighbors co)
    outer = filter (isOutside cubes bounds) free

getBounds :: Set Coord -> (Int, Int)
getBounds = foldl update (0, 0)
  where
    update (mi, ma) (x, y, z) = (minimum [mi, x, y, z], maximum [ma, x, y, z])

neighbors :: Coord -> [Coord]
neighbors (x, y, z) =
  [ (x - 1, y, z),
    (x + 1, y, z),
    (x, y - 1, z),
    (x, y + 1, z),
    (x, y, z - 1),
    (x, y, z + 1)
  ]

isOutside :: Set Coord -> (Int, Int) -> Coord -> Bool
isOutside cubes bounds co = isNothing $ outsideClosure cubes bounds coSet coSet
  where
    coSet = S.fromList [co]

outsideClosure :: Set Coord -> (Int, Int) -> Set Coord -> Set Coord -> Maybe (Set Coord)
outsideClosure _ bounds cs _ | any (outOfBounds bounds) cs = Nothing
outsideClosure _ bounds cs _ | any (outOfBounds bounds) cs = Nothing
outsideClosure cubes bounds cs closure =
  if closure' == closure
    then Just closure
    else outsideClosure cubes bounds new closure'
  where
    ns = S.fromList $ concatMap neighbors cs
    free = S.filter (`S.notMember` cubes) ns
    new = S.filter (`S.notMember` closure) free
    closure' = S.union closure new

outOfBounds :: (Int, Int) -> Coord -> Bool
outOfBounds (mi, ma) (x, y, z) = oob x || oob y || oob z
  where
    oob a = a < mi || a > ma
