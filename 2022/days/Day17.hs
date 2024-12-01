module Main where

import Aoc (Parser, getParsedInput)
import Control.Monad.State (State, get, gets, put, runState)
import Data.List (intersect, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Megaparsec (many, (<|>))
import Text.Megaparsec.Char (char)

main :: IO ()
main = do
  input <- getParsedInput 17 jetPattern
  print $ rockHeight 2022 input
  print $ rockHeight 1000000000000 input

type Pos = (Int, Int)

type Rock = [Pos]

data Jet = JLeft | JRight
  deriving (Show)

data Cave = Cave
  { cTop :: [[Int]],
    cJets :: [(Int, Jet)],
    cHashes :: Map [Int] (Int, Int),
    cRounds :: Int
  }

jetPattern :: Parser [Jet]
jetPattern = many (left <|> right)
  where
    left = JLeft <$ char '<'
    right = JRight <$ char '>'

rockHeight :: Int -> [Jet] -> Int
rockHeight count jets = case result of
  Just r -> r
  Nothing -> heighestRock (cTop cave')
  where
    top = replicate 7 [0]
    jets' = cycle $ zip [0 ..] jets
    cave = Cave top jets' M.empty count
    rocks = take count genRocks
    (result, cave') = runState (runRocks rocks) cave

genRocks :: [(Int, Rock)]
genRocks = zip [0 ..] $ cycle [minus, plus, wedge, bar, square]
  where
    minus = [(0, 0), (1, 0), (2, 0), (3, 0)]
    plus = [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
    wedge = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
    bar = [(0, 0), (0, 1), (0, 2), (0, 3)]
    square = [(0, 0), (0, 1), (1, 0), (1, 1)]

move :: Pos -> Pos -> Pos
move (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

heighestRock :: [[Int]] -> Int
heighestRock = maximum . map head

runRocks :: [(Int, Rock)] -> State Cave (Maybe Int)
runRocks [] = return Nothing
runRocks (r : rs) = do
  result <- runRock r
  case result of
    Just res -> return $ Just res
    Nothing -> runRocks rs

runRock :: (Int, Rock) -> State Cave (Maybe Int)
runRock (idx, rock) = do
  result <- checkShortcut idx
  case result of
    Just res -> return $ Just res
    Nothing -> do
      top <- gets cTop
      let startY = heighestRock top + 4
      let startRock = map (move (2, startY)) rock
      endRock <- iterRock startRock
      putRock endRock
      return Nothing

putRock :: Rock -> State Cave ()
putRock rock = do
  cave <- get
  let top = foldl topUpdate (cTop cave) rock
  put cave {cTop = top}
  where
    topUpdate top (x, y) = take x top ++ [xUpdate (top !! x) y] ++ drop (x + 1) top
    xUpdate col y = take 20 (sortBy (flip compare) (y : col))

nextJet :: State Cave (Int, Jet)
nextJet = do
  cave <- get
  let jets = cJets cave
  put $ cave {cJets = tail jets}
  return $ head jets

iterRock :: Rock -> State Cave Rock
iterRock rock = do
  rock' <- blowRock rock
  rock'' <- fallRock rock'
  if rock' == rock''
    then return rock'
    else iterRock rock''

blowRock :: Rock -> State Cave Rock
blowRock rock = do
  jet <- nextJet
  top <- gets cTop
  let m = case snd jet of
        JLeft -> (-1, 0)
        JRight -> (1, 0)
  let rock' = map (move m) rock
  return $ if isPositionLegal rock' top then rock' else rock

fallRock :: Rock -> State Cave Rock
fallRock rock = do
  top <- gets cTop
  let rock' = map (move (0, -1)) rock
  return $ if isPositionLegal rock' top then rock' else rock

isPositionLegal :: Rock -> [[Int]] -> Bool
isPositionLegal rock top = not (hitsWall || hitsTop)
  where
    topPs = concatMap (\(x, ys) -> [(x, y) | y <- ys]) (zip [0 ..] top)
    hitsWall = any (\(x, _) -> x < 0 || x > 6) rock
    hitsTop = not $ null (topPs `intersect` rock)

checkShortcut :: Int -> State Cave (Maybe Int)
checkShortcut rnd = do
  totalRounds <- gets cRounds
  prev <- checkStateHash rnd
  top <- gets cTop
  let now = (rnd, heighestRock top)
  case prev of
    Just prev' -> return $ shortcut totalRounds now prev'
    Nothing -> return Nothing

shortcut :: Int -> (Int, Int) -> (Int, Int) -> Maybe Int
shortcut total (nRound, nHeight) (pRound, pHeight) =
  if fits
    then Just (nHeight + loopCount * heightDiff)
    else Nothing
  where
    remaining = total - nRound
    loopSize = nRound - pRound
    fits = remaining `mod` loopSize == 0
    loopCount = remaining `div` loopSize
    heightDiff = nHeight - pHeight

checkStateHash :: Int -> State Cave (Maybe (Int, Int))
checkStateHash rnd = do
  cave <- get
  let hashes = cHashes cave
  let hash = stateHash rnd cave
  case M.lookup hash hashes of
    Just res -> return $ Just res
    Nothing -> do
      let height = heighestRock (cTop cave) 
      let hashes' = M.insert hash (rnd, height) hashes
      put cave {cHashes = hashes'}
      return Nothing

stateHash :: Int -> Cave -> [Int]
stateHash rnd cave = [rockIdx, jetIdx] ++ topline'
  where
    rockIdx = rnd `mod` 5
    jetIdx = (fst . head . cJets) cave
    topline = map head (cTop cave)
    topline' = map (subtract $ minimum topline) topline
