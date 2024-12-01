module Main where

import Aoc (Parser, getParsedInput, numberP)
import Data.Complex (Complex (..), imagPart, realPart)
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Text.Megaparsec (choice, some, someTill, (<|>))
import Text.Megaparsec.Char (char, newline)

main :: IO ()
main = do
  (grid, moves) <- getParsedInput 22 inputP
  print $ password (walkGrid grid translation1 moves)
  print $ password (walkGrid grid translation2 moves)

-- print $ password (walkGrid grid translation3 moves)

type Trans = Map (Int, Int, Face) (Int, Int, Face)

translation1 :: Trans
translation1 = M.fromList $ concat maps
  where
    maps =
      [ [((x, 99, U), (x, 199, U)) | x <- [0 .. 49]],
        [((x, 200, D), (x, 100, D)) | x <- [0 .. 49]],
        [((x, -1, U), (x, 149, U)) | x <- [50 .. 99]],
        [((x, 150, D), (x, 0, D)) | x <- [50 .. 99]],
        [((x, -1, U), (x, 49, U)) | x <- [100 .. 149]],
        [((x, 50, D), (x, 0, D)) | x <- [100 .. 149]],
        [((49, y, L), (149, y, L)) | y <- [0 .. 49]],
        [((150, y, R), (50, y, R)) | y <- [0 .. 49]],
        [((49, y, L), (99, y, L)) | y <- [50 .. 99]],
        [((100, y, R), (50, y, R)) | y <- [50 .. 99]],
        [((-1, y, L), (99, y, L)) | y <- [100 .. 149]],
        [((100, y, R), (0, y, R)) | y <- [100 .. 149]],
        [((-1, y, L), (49, y, L)) | y <- [150 .. 199]],
        [((50, y, R), (0, y, R)) | y <- [150 .. 199]]
      ]

translation2 :: Trans
translation2 = M.fromList $ concat maps
  where
    maps =
      [ [((x, 99, U), (50, x + 50, R)) | x <- [0 .. 49]],
        [((x, 200, D), (x + 100, 0, D)) | x <- [0 .. 49]],
        [((x, -1, U), (0, x + 100, R)) | x <- [50 .. 99]],
        [((x, 150, D), (49, x + 100, L)) | x <- [50 .. 99]],
        [((x, -1, U), (x - 100, 199, U)) | x <- [100 .. 149]],
        [((x, 50, D), (99, x - 50, L)) | x <- [100 .. 149]],
        [((49, y, L), (0, -(y - 149), R)) | y <- [0 .. 49]],
        [((150, y, R), (99, -(y - 149), L)) | y <- [0 .. 49]],
        [((49, y, L), (y - 50, 100, D)) | y <- [50 .. 99]],
        [((100, y, R), (y + 50, 49, U)) | y <- [50 .. 99]],
        [((-1, y, L), (50, -(y - 149), R)) | y <- [100 .. 149]],
        [((100, y, R), (149, -(y - 149), L)) | y <- [100 .. 149]],
        [((-1, y, L), (y - 100, 0, D)) | y <- [150 .. 199]],
        [((50, y, R), (y - 100, 149, U)) | y <- [150 .. 199]]
      ]

-- translation3 :: Trans
-- translation3 = M.fromList $ concat maps
--   where
--     maps =
--       [ [((x, 3), (8, x - 4, R)) | x <- [4 .. 7]],
--         [((x, 12), (-(x - 11), 7, U)) | x <- [8 .. 11]],
--         [((12, y), (-(y - 19), 8, D)) | y <- [4 .. 7]]
--       ]

type Grid = [[Maybe Tile]]

data Tile = Free | Wall
  deriving (Show, Eq)

data Move = Walk Int | Turn Dir
  deriving (Show)

type Dir = Complex Float

data Face = L | R | U | D
  deriving (Show, Ord, Eq)

type Pos = Complex Float

type State = (Pos, Dir)

inputP :: Parser (Grid, [Move])
inputP = do
  gridLines <- some gridLine
  _ <- newline
  moves <- someTill moveP newline
  return (gridLines, moves)
  where
    gridLine = someTill tileP newline

tileP :: Parser (Maybe Tile)
tileP =
  choice
    [ Just Free <$ char '.',
      Just Wall <$ char '#',
      Nothing <$ char ' '
    ]

moveP :: Parser Move
moveP = (Walk <$> numberP) <|> (Turn <$> dirP)
  where
    dirP = (0 :+ 1 <$ char 'R') <|> (0 :+ (-1) <$ char 'L')

-- padGrid :: Grid -> Grid
-- padGrid grid = [pad r | r <- grid]
--   where
--     maxLen = maximum [length r | r <- grid]
--     pad r = r ++ replicate (maxLen - length r) Empty

walkGrid :: Grid -> Trans -> [Move] -> State
walkGrid grid trans moves = walkGrid' grid trans moves start
  where
    minX = fromJust $ elemIndex (Just Free) (head grid)
    start = (fromIntegral minX :+ 0, 1 :+ 0)

walkGrid' :: Grid -> Trans -> [Move] -> State -> State
walkGrid' _ _ [] state = state
walkGrid' grid trans (m : ms) state = walkGrid' grid trans ms state'
  where
    state' = move grid trans m state

move :: Grid -> Trans -> Move -> State -> State
move _ _ (Turn t) (pos, dir) = (pos, dir * t)
move grid trans (Walk n) state = walk grid trans n state

walk :: Grid -> Trans -> Int -> State -> State
walk _ _ 0 state = state
walk grid trans n state = case step grid trans state of
  Just state' -> walk grid trans (n - 1) state'
  Nothing -> state

step :: Grid -> Trans -> State -> Maybe State
step grid trans (pos, dir) = case gridAt grid pos' of
  Just Free -> Just (pos', dir')
  Just Wall -> Nothing
  Nothing -> error "unreachable"
  where
    (pos', dir') = translate trans (pos + dir, dir)

posX :: Pos -> Int
posX = round . realPart

posY :: Pos -> Int
posY = round . imagPart

gridAt :: Grid -> Pos -> Maybe Tile
gridAt grid pos = do
  row <- if length grid > y then Just (grid !! y) else Nothing
  if length row > x then row !! x else Nothing
  where
    x = posX pos
    y = posY pos

translate :: Trans -> State -> State
translate trans (pos, dir) = case M.lookup (x, y, face) trans of
  Just (x', y', face') ->
    let pos' = fromIntegral x' :+ fromIntegral y'
     in (pos', faceToDir face')
  Nothing -> (pos, dir)
  where
    x = posX pos
    y = posY pos
    face = dirToFace dir

faceToDir :: Face -> Dir
faceToDir L = (-1) :+ 0
faceToDir R = 1 :+ 0
faceToDir U = 0 :+ (-1)
faceToDir D = 0 :+ 1

dirToFace :: Dir -> Face
dirToFace dir = case dir of
  (-1) :+ 0 -> L
  1 :+ 0 -> R
  0 :+ (-1) -> U
  0 :+ 1 -> D
  _ -> undefined

password :: State -> Int
password (pos, dir) = 1000 * row + 4 * col + facing
  where
    row = posY pos + 1
    col = posX pos + 1
    facing = case dir of
      1 :+ 0 -> 0
      0 :+ 1 -> 1
      -1 :+ 0 -> 2
      0 :+ -1 -> 3
      _ -> error "invalid facing"
