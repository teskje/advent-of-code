module Main where

import Aoc (Parser, getParsedLines, numberP)
import Control.Monad.Reader (Reader, ask, runReader)
import Data.List (sort)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Text.Megaparsec (many, sepBy, (<|>))
import Text.Megaparsec.Char (string, upperChar)

main :: IO ()
main = do
  input <- getParsedLines 16 valveLine
  let tunnels = [(v, t) | (v, _, t) <- input]
  let ds = findDistances tunnels
  let rs = M.fromList [(v, r) | (v, r, _) <- input, r > 0]
  let info = Info {dists = ds, frates = rs}
  print $ runReader part1 info
  print $ runReader part2 info

type V = String

type Path = [V]

type Dists = Map (V, V) Int

type FRates = Map V Int

data Info = Info
  { dists :: Dists,
    frates :: FRates
  }

valveLine :: Parser (V, Int, [V])
valveLine = do
  _ <- string "Valve "
  valve <- valveP
  _ <- string " has flow rate="
  flow <- numberP
  _ <-
    string "; tunnels lead to valves "
      <|> string "; tunnel leads to valve "
  tunnels <- sepBy valveP (string ", ")
  return (valve, flow, tunnels)
  where
    valveP = many upperChar

findDistances :: [(V, [V])] -> Dists
findDistances tunnels = findDistances' tunnels ds
  where
    ds = M.fromList [((a, a), 0) | (a, _) <- tunnels]

findDistances' :: [(V, [V])] -> Dists -> Dists
findDistances' tunnels ds
  | ds' == ds = ds
  | otherwise = findDistances' tunnels ds'
  where
    ds' = stepDistances tunnels ds

stepDistances :: [(V, [V])] -> Dists -> Dists
stepDistances tunnels ds = foldl stepTunnel ds tunnels

stepTunnel :: Dists -> (V, [V]) -> Dists
stepTunnel ds tunnel = newDs
  where
    newDs = extendTunnel ds tunnel

extendTunnel :: Dists -> (V, [V]) -> Dists
extendTunnel ds (ta, tbs) = M.union ds newDs
  where
    extend ((a, b), d)
      | b == ta = [((a, tb), d + 1) | tb <- tbs]
      | otherwise = []
    newDs = M.fromList $ concatMap extend (M.assocs ds)

part1 :: Reader Info Int
part1 = do
  flows <- findFlows 30 ["AA"]
  return $ (fst . maximum) flows

part2 :: Reader Info Int
part2 = do
  flows1 <- findFlows 26 ["AA"]
  let flows1' = reduceFlows flows1
  flows2 <- findFlows2 $ M.assocs flows1'
  return $ maximum flows2

findFlows :: Int -> Path -> Reader Info [(Int, Path)]
findFlows budget p = do
  info <- ask
  let rs = frates info
  let nexts = [v | (v, _) <- M.assocs rs, v `notElem` p]
  let ps = [p ++ [v] | v <- nexts]
  flows <- traverse (flowPath budget) ps
  let pathFlows = mapMaybe packMaybe (zip flows ps)
  pathFlows' <- concat <$> traverse (findFlows budget) ps
  if null pathFlows
    then return []
    else return $ pathFlows ++ pathFlows'

reduceFlows :: [(Int, Path)] -> Map Path Int
reduceFlows [] = M.empty
reduceFlows ((r, _) : fs) | r <= 1000 = reduceFlows fs
reduceFlows ((r, p) : fs) =
  if M.member p' m
    then M.adjust (max r) p' m
    else M.insert p' r m
  where
    m = reduceFlows fs
    p' = sort p

findFlows2 :: [(Path, Int)] -> Reader Info [Int]
findFlows2 [] = return []
findFlows2 ((p1, r1) : rest1) = do
  info <- ask
  let rs = M.filterWithKey (\k _ -> k `notElem` p1) (frates info)
  let info' = info {frates = rs}
  let flows2 = runReader (findFlows 26 ["AA"]) info'
  let r2 = (fst . maximum) flows2
  let r = r1 + r2
  rest2 <- findFlows2 rest1
  return $ r : rest2

packMaybe :: (Maybe a, b) -> Maybe (a, b)
packMaybe (Just a, b) = Just (a, b)
packMaybe (Nothing, _) = Nothing

flowPath :: Int -> Path -> Reader Info (Maybe Int)
flowPath budget _ | budget < 0 = return Nothing
flowPath _ [] = error "unreachable"
flowPath _ [_] = return (Just 0)
flowPath budget (a : b : p) = do
  info <- ask
  let cost = dists info ! (a, b) + 1
  let budget' = budget - cost
  let rate = frates info ! b
  maybeFlow <- flowPath budget' (b : p)
  return $ (budget' * rate +) <$> maybeFlow
