module Main where

import Aoc (Parser, getParsedInput, numberP)
import Data.List (sort)
import Text.Megaparsec (between, sepBy, (<|>))
import Text.Megaparsec.Char (char, newline)

main :: IO ()
main = do
  input <- getParsedInput 13 listsList
  print $ sum (orderedIndexes input)
  print $ product (dividerIndexes input)

data Item = Num Int | Lst [Item]
  deriving (Show, Eq)

instance Ord Item where
  compare (Num a) (Num b) = compare a b
  compare a@(Num _) b@(Lst _) = compare (Lst [a]) b
  compare a@(Lst _) b@(Num _) = compare a (Lst [b])
  compare (Lst a) (Lst b) = compare a b

listsList :: Parser [(Item, Item)]
listsList = sepBy itemPair newline

itemPair :: Parser (Item, Item)
itemPair = do
  a <- itemP <* newline
  b <- itemP <* newline
  return (a, b)

itemP :: Parser Item
itemP = list <|> number
  where
    number = Num <$> numberP
    list = Lst <$> listP

listP :: Parser [Item]
listP = between (char '[') (char ']') inner
  where
    inner = sepBy itemP (char ',')

orderedIndexes :: [(Item, Item)] -> [Int]
orderedIndexes pairs = [i | (i, (a, b)) <- zip [1 ..] pairs, a < b]

dividerIndexes :: [(Item, Item)] -> [Int]
dividerIndexes pairs = [i | (i, p) <- zip [1 ..] pkts, p `elem` dividers]
  where
    flat = concatMap (\(a, b) -> [a, b]) pairs
    dividers = [Lst [Lst [Num 2]], Lst [Lst [Num 6]]]
    pkts = sort $ flat ++ dividers
