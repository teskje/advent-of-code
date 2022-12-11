module Main where

import Aoc (Parser, getParsedInput)
import Data.List (sortBy)
import Text.Megaparsec (many, optional, sepBy, try, (<|>))
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

main :: IO ()
main = do
  input <- getParsedInput 11 monkeyList
  print $ part1 input
  print $ part2 input

part1 :: ([Monkey], [Hand]) -> Int
part1 (mks, hands) = monkeyBusiness cts
  where
    mks' = map (monkeyMapOp (`div` 3)) mks
    (_, cts) = runMonkeysN 20 hands mks'

part2 :: ([Monkey], [Hand]) -> Int
part2 (mks, hands) = monkeyBusiness cts
  where
    r = product $ map mkDivisor mks
    mks' = map (monkeyMapOp (`mod` r)) mks
    (_, cts) = runMonkeysN 10000 hands mks'

data Monkey = Monkey
  { mkIdx :: Int,
    mkOp :: Int -> Int,
    mkDivisor :: Int,
    mkTrueTarget :: Int,
    mkFalseTarget :: Int
  }

monkeyMapOp :: (Int -> Int) -> Monkey -> Monkey
monkeyMapOp f (Monkey idx op d tt ft) = Monkey idx (f . op) d tt ft

type Hand = [Int]

type Count = Int

monkeyBusiness :: [Count] -> Int
monkeyBusiness = product . take 2 . sortBy (flip compare)

monkeyList :: Parser ([Monkey], [Hand])
monkeyList = unzip <$> many monkeyP

monkeyP :: Parser (Monkey, Hand)
monkeyP = do
  idx <- string "Monkey " *> decimal <* char ':' <* newline
  hand <- string "  Starting items: " *> intList <* newline
  op <- string "  Operation: " *> opP <* newline
  test <- string "  Test: divisible by " *> decimal <* newline
  ifTrue <- string "    If true: throw to monkey " *> decimal <* newline
  ifFalse <- string "    If false: throw to monkey " *> decimal <* newline
  _ <- optional newline
  let monkey = Monkey idx op test ifTrue ifFalse
  return (monkey, hand)

intList :: Parser [Int]
intList = decimal `sepBy` string ", "

opP :: Parser (Int -> Int)
opP = string "new = old " *> (add <|> try mul <|> sqr)
  where
    add = (+) <$ string "+ " <*> decimal
    mul = (*) <$ string "* " <*> decimal
    sqr = (\x -> x * x) <$ string "* old"

runMonkeysN :: Int -> [Hand] -> [Monkey] -> ([Hand], [Count])
runMonkeysN n hands mks = runMonkeys hands mks'
  where
    mks' = take (length mks * n) (cycle mks)

runMonkeys :: [Hand] -> [Monkey] -> ([Hand], [Count])
runMonkeys hands = runMonkeys' cts hands
  where
    cts = replicate (length hands) 0

runMonkeys' :: [Count] -> [Hand] -> [Monkey] -> ([Hand], [Count])
runMonkeys' cts hands [] = (hands, cts)
runMonkeys' cts hands (mk : mks) = runMonkeys' cts' hands' mks
  where
    hands' = monkeyMove hands mk
    idx = mkIdx mk
    ct = length $ hands !! idx
    cts' = modifyAt idx (+ ct) cts

monkeyMove :: [Hand] -> Monkey -> [Hand]
monkeyMove hands mk = foldl (monkeyThrow mk) hands' toThrow
  where
    idx = mkIdx mk
    toThrow = hands !! idx
    hands' = modifyAt idx (const []) hands

monkeyThrow :: Monkey -> [Hand] -> Int -> [Hand]
monkeyThrow mk hands item = modifyAt idx (++ [item']) hands
  where
    item' = mkOp mk item
    idx = monkeyInspect mk item'

monkeyInspect :: Monkey -> Int -> Int
monkeyInspect mk i =
  if i `mod` mkDivisor mk == 0
    then mkTrueTarget mk
    else mkFalseTarget mk

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs
