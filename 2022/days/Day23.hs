module Main where

import Aoc (getInputLines)
import Data.List (elemIndices)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  input <- getInputLines 23
  let field = makeField input
  let dirs = cycle [N, S, W, E]
  print $ area (runFor 10 field dirs)
  print $ runToEnd field dirs

type Pos = (Int, Int)

type Field = Set Pos

data Dir = N | S | W | E
  deriving (Show)

makeField :: [[Char]] -> Field
makeField = foldl addRow S.empty . zip [0 ..]
  where
    addRow f (y, row) = S.union f (makeRow y row)
    makeRow y row = S.fromList [(x, y) | x <- elemIndices '#' row]

runFor :: Int -> Field -> [Dir] -> Field
runFor 0 field _ = field
runFor n field dirs = runFor (n - 1) field' dirs'
  where
    (field', dirs') = runRound field dirs

runToEnd :: Field -> [Dir] -> Int
runToEnd field dirs =
  if field == field'
    then 1
    else 1 + runToEnd field' dirs'
  where
    (field', dirs') = runRound field dirs

runRound :: Field -> [Dir] -> (Field, [Dir])
runRound field dirs = (field', tail dirs)
  where
    props = roundPart1 field (take 4 dirs)
    field' = roundPart2 props

roundPart1 :: Field -> [Dir] -> Map Pos [Pos]
roundPart1 field dirs = foldl (propose field dirs) M.empty field

propose :: Field -> [Dir] -> Map Pos [Pos] -> Pos -> Map Pos [Pos]
propose field dirs props pos = M.insertWith (++) prop [pos] props
  where
    free = mapMaybe (proposeDir field pos) dirs
    prop = case length free of
      0 -> pos
      4 -> pos
      _ -> head free

proposeDir :: Field -> Pos -> Dir -> Maybe Pos
proposeDir field pos dir =
  if length empty == 3
    then Just (fields !! 1)
    else Nothing
  where
    fields = fieldsInDir pos dir
    empty = filter (`S.notMember` field) fields

fieldsInDir :: Pos -> Dir -> [Pos]
fieldsInDir (x, y) N = [(x', y - 1) | x' <- [x - 1, x, x + 1]]
fieldsInDir (x, y) S = [(x', y + 1) | x' <- [x - 1, x, x + 1]]
fieldsInDir (x, y) W = [(x - 1, y') | y' <- [y - 1, y, y + 1]]
fieldsInDir (x, y) E = [(x + 1, y') | y' <- [y - 1, y, y + 1]]

roundPart2 :: Map Pos [Pos] -> Field
roundPart2 props = foldl addProps S.empty (M.assocs props)
  where
    addProps field (prop, elves) =
      if length elves == 1
        then S.insert prop field
        else S.union field (S.fromList elves)

area :: Field -> Int
area field = dx * dy - length field
  where
    xs = S.map fst field
    ys = S.map snd field
    dx = maximum xs - minimum xs + 1
    dy = maximum ys - minimum ys + 1
