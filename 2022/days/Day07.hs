module Main where

import Aoc (Parser, getParsedInput, numberP)
import Data.List (find, isPrefixOf, sort)
import Data.Maybe (fromJust)
import Text.Megaparsec (many, manyTill, (<|>))
import Text.Megaparsec.Char (asciiChar, char, newline, string)

main :: IO ()
main = do
  input <- getParsedInput 7 commands
  let listing = buildListing input
  let totals = sort (buildTotals listing)
  print $ (sum . filter (<= 100000)) totals

  let freeSpace = 70000000 - last totals
  let missing = 30000000 - freeSpace
  print $ (fromJust . find (>= missing)) totals

data Command = Cd String | Ls Int
  deriving (Show)

commands :: Parser [Command]
commands = many (cdCommand <|> lsCommand)

cdCommand :: Parser Command
cdCommand = Cd <$ string "$ cd " <*> manyTill asciiChar newline

lsCommand :: Parser Command
lsCommand = do
  _ <- string "$ ls\n"
  sizes <- many lsLine
  return $ Ls (sum sizes)

lsLine :: Parser Int
lsLine = lsFile <|> lsDir
  where
    lsFile = numberP <* char ' ' <* manyTill asciiChar newline
    lsDir = 0 <$ string "dir " <* manyTill asciiChar newline

type Path = [String]

type Listing = [(Path, Int)]

buildListing :: [Command] -> Listing
buildListing = buildListing' []

buildListing' :: Path -> [Command] -> [(Path, Int)]
buildListing' _ [] = []
buildListing' r (Cd ".." : cs) = buildListing' (init r) cs
buildListing' r (Cd d : Ls s : cs) = (r', s) : buildListing' r' cs
  where
    r' = r ++ [d]
buildListing' r (Cd d : cs) = (r', 0) : buildListing' r' cs
  where
    r' = r ++ [d]
buildListing' _ (Ls _ : _) = error "unreachable"

buildTotals :: [(Path, Int)] -> [Int]
buildTotals listing = map (totalDirSize listing) paths
  where
    paths = map fst listing

totalDirSize :: Listing -> Path -> Int
totalDirSize listing p = sum $ map (subDirSize p) listing
  where
    subDirSize p1 (p2, s) = if p1 `isPrefixOf` p2 then s else 0
