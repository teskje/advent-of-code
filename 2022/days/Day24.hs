module Main where

import Aoc (Parser, getParsedLines)
import Data.List (elemIndex, elemIndices, find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec (choice, many)
import Text.Megaparsec.Char (char)

main :: IO ()
main = do
  input <- getParsedLines 24 gridRow
  let field = makeField input
  let (start, end) = findPoints input
  print $ findPath1 field start end
  print $ findPath2 field start end

data Tile = Free | Storm Dir | Wall
  deriving (Show, Eq)

data Dir = L | R | U | D
  deriving (Show, Eq)

type Pos = (Int, Int)

type Path = (Pos, Int)

data Field = Field
  { fStorms :: Map Pos [Dir],
    fWalls :: Set Pos,
    fMaxX :: Int,
    fMaxY :: Int
  }
  deriving (Show)

fieldAt :: Field -> Pos -> Tile
fieldAt field (x, y)
  | x < 0 = Wall
  | x > fMaxX field = Wall
  | y < 0 = Wall
  | y > fMaxY field = Wall
  | S.member (x, y) (fWalls field) = Wall
  | M.member (x, y) (fStorms field) = Storm L
  | otherwise = Free

gridRow :: Parser [Tile]
gridRow = many tile
  where
    tile =
      choice
        [ Free <$ char '.',
          Storm L <$ char '<',
          Storm R <$ char '>',
          Storm U <$ char '^',
          Storm D <$ char 'v',
          Wall <$ char '#'
        ]

makeField :: [[Tile]] -> Field
makeField grid = Field (M.fromList storms) (S.fromList walls) maxX maxY
  where
    iGrid = zip [0 ..] grid
    getStorm (x, Storm dir) = Just (x, dir)
    getStorm _ = Nothing
    findStorms = mapMaybe getStorm . zip [0 ..]
    storms = concatMap (\(y, r) -> [((x, y), [dir]) | (x, dir) <- findStorms r]) iGrid
    walls = concatMap (\(y, r) -> [(x, y) | x <- elemIndices Wall r]) iGrid
    maxX = length (head grid) - 1
    maxY = length grid - 1

findPoints :: [[Tile]] -> (Pos, Pos)
findPoints grid = (start, end)
  where
    freeIn = fromJust . elemIndex Free
    start = (freeIn (head grid), 0)
    end = (freeIn (last grid), length grid - 1)

findPath1 :: Field -> Pos -> Pos -> Path
findPath1 field start end = path
  where
    (path, _) = findPath' field (S.fromList [(start, 0)]) end

findPath2 :: Field -> Pos -> Pos -> Path
findPath2 field start end = path3
  where
    (path1, field1) = findPath' field (S.fromList [(start, 0)]) end
    (path2, field2) = findPath' field1 (S.fromList [path1]) start
    (path3, _) = findPath' field2 (S.fromList [path2]) end

findPath' :: Field -> Set Path -> Pos -> (Path, Field)
findPath' field paths end = case pathTo end paths of
  Just path -> (path, field)
  Nothing ->
    let (field', paths') = step field paths
     in findPath' field' paths' end

pathTo :: Pos -> Set Path -> Maybe Path
pathTo end = find ((== end) . fst)

step :: Field -> Set Path -> (Field, Set Path)
step field paths = (field', paths')
  where
    field' = moveStorms field
    paths' = extendPaths field' paths

moveStorms :: Field -> Field
moveStorms field = field {fStorms = storms'}
  where
    storms = fStorms field
    stormList = concatMap (\(p, ds) -> [(p, d) | d <- ds]) (M.assocs storms)
    storms' = foldl (moveStorm field) M.empty stormList

moveStorm :: Field -> Map Pos [Dir] -> (Pos, Dir) -> Map Pos [Dir]
moveStorm field storms (pos, dir) = M.insertWith (++) pos' [dir] storms
  where
    pos' = nextStormPos field pos dir

nextStormPos :: Field -> Pos -> Dir -> Pos
nextStormPos field pos dir = case fieldAt field pos' of
  Wall -> nextStormPos field pos' dir
  _ -> pos'
  where
    pos' = wrapAround field $ move pos dir

move :: Pos -> Dir -> Pos
move (x, y) dir = case dir of
  L -> (x - 1, y)
  R -> (x + 1, y)
  U -> (x, y - 1)
  D -> (x, y + 1)

wrapAround :: Field -> Pos -> Pos
wrapAround field (x, y)
  | x < 0 = (maxX, y)
  | x > maxX = (0, y)
  | y < 0 = (x, maxY)
  | y > maxY = (x, 0)
  | otherwise = (x, y)
  where
    maxX = fMaxX field
    maxY = fMaxY field

extendPaths :: Field -> Set Path -> Set Path
extendPaths field = S.fromList . concatMap (extendPath field)

extendPath :: Field -> Path -> [Path]
extendPath field (pos, i) = [(pos', i + 1) | pos' <- free]
  where
    candidates = pos : map (move pos) [L, R, U, D]
    free = filter ((== Free) . fieldAt field) candidates
