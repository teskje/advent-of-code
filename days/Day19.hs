module Main where

import Aoc (Parser, getParsedLines, numberP)
import Data.List (sortOn, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, mapMaybe)
import Text.Megaparsec (option, optional, try)
import Text.Megaparsec.Char (char, string)

main :: IO ()
main = do
  input <- getParsedLines 19 blueprint
  print $ sum (map (simulateQuality 24) input)
  print $ product (map (simulate 32) (take 3 input))

data Res = Ore | Clay | Obsidian | Geode
  deriving (Eq, Ord, Show)

type Resources = Map Res Int

type Robots = Map Res Int

data Blueprint = Blueprint Int (Map Res Resources)
  deriving (Show)

data State = State
  { sResources :: Resources,
    sRobots :: Robots,
    sBuilding :: Maybe Res
  }
  deriving (Show)

resList :: [Res]
resList = [Ore, Clay, Obsidian, Geode]

resMap :: a -> a -> a -> a -> Map Res a
resMap ore clay obsidian geode =
  M.fromList [(Ore, ore), (Clay, clay), (Obsidian, obsidian), (Geode, geode)]

blueprint :: Parser Blueprint
blueprint = do
  id' <- string "Blueprint " *> numberP <* char ':'
  ore <- string " Each ore robot costs " *> resourcesP <* char '.'
  clay <- string " Each clay robot costs " *> resourcesP <* char '.'
  obsidian <- string " Each obsidian robot costs " *> resourcesP <* char '.'
  geode <- string " Each geode robot costs " *> resourcesP <* char '.'
  let ress = resMap ore clay obsidian geode
  return $ Blueprint id' ress

resourcesP :: Parser Resources
resourcesP = do
  ore <- option 0 (try $ numberP <* string " ore")
  _ <- optional (string " and ")
  clay <- option 0 (try $ numberP <* string " clay")
  _ <- optional (string " and ")
  obsidian <- option 0 (try $ numberP <* string " obsidian")
  let res = resMap ore clay obsidian 0
  return res

simulateQuality :: Int -> Blueprint -> Int
simulateQuality minutes bp = id' * simulate minutes bp
  where
    Blueprint id' _ = bp

simulate :: Int -> Blueprint -> Int
simulate minutes bp = geodes
  where
    startState = State (resMap 0 0 0 0) (resMap 1 0 0 0) Nothing
    endStates = iter (step bp) minutes [startState]
    geodes = maximum $ map ((M.! Geode) . sResources) endStates

iter :: (a -> a) -> Int -> a -> a
iter _ 0 s = s
iter f i s = iter f (i - 1) (f s)

step :: Blueprint -> [State] -> [State]
step bp = pruneStates . concatMap (step1 bp)

step1 :: Blueprint -> State -> [State]
step1 bp s = s1 : s2
  where
    State res ros building = s
    prevBuildable = [r | r <- resList, buildable r bp res]
    candidates = resList \\ prevBuildable
    ros' = case building of
      Just r -> M.adjust (+ 1) r ros
      Nothing -> ros
    s1 = State (workRobots res ros) ros' Nothing
    s2 = mapMaybe (\r -> buildRobot r bp s1) candidates

workRobots :: Resources -> Robots -> Resources
workRobots res ros = res'
  where
    work r = res M.! r + ros M.! r
    res' = M.fromList $ zip resList (map work resList)

buildable :: Res -> Blueprint -> Resources -> Bool
buildable r (Blueprint _ costs) res = isJust $ useResources res cost
  where
    cost = costs M.! r

buildRobot :: Res -> Blueprint -> State -> Maybe State
buildRobot r (Blueprint _ costs) s = result
  where
    maybeRes = useResources (sResources s) (costs M.! r)
    result = fmap (\res -> s {sResources = res, sBuilding = Just r}) maybeRes

useResources :: Resources -> Resources -> Maybe Resources
useResources res cost =
  if all (>= 0) res'
    then Just $ M.fromList (zip resList res')
    else Nothing
  where
    res' = [res M.! r - cost M.! r | r <- resList]

pruneStates :: [State] -> [State]
pruneStates = take 5000 . sortOn (negate . rateState)

rateState :: State -> Int
rateState (State res ros building) = roScore + reScore
  where
    ros' = case building of
      Just r -> M.adjust (+ 1) r ros
      Nothing -> ros
    roScoring = [(Ore, 1), (Clay, 2), (Obsidian, 4), (Geode, 8)]
    roScore = sum [ros' M.! r * x | (r, x) <- roScoring]
    reScoring = [(Ore, 0), (Clay, 0), (Obsidian, 0), (Geode, 10)]
    reScore = sum [res M.! r * x | (r, x) <- reScoring]
