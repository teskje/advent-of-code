module Main where

import Aoc (Parser, getParsedLines, numberP)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Text.Megaparsec (choice, count, (<|>))
import Text.Megaparsec.Char (char, lowerChar, space, string)

main :: IO ()
main = do
  input <- getParsedLines 21 monkeyP
  let (nums1, comps1) = M.mapEither id (M.fromList input)
  let (nums1', _) = eval nums1 comps1
  print $ nums1' M.! "root"

  let nums2 = (M.insert "root" 0 . M.delete "humn") nums1
  let comps2 = M.adjust (\(a, _, b) -> (a, Sub, b)) "root" comps1
  let (nums2', comps2') = eval nums2 comps2
  let comps2'' = invertComps nums2' comps2'
  let (nums2'', _) = eval nums2' comps2''
  print $ nums2'' M.! "humn"

type Job = Either Int Comp

type Comp = (String, Op, String)

data Op = Add | Sub | Mul | Div
  deriving (Show)

monkeyP :: Parser (String, Job)
monkeyP = do
  name <- nameP
  _ <- string ": "
  job <- Left <$> numberP <|> Right <$> compP
  return (name, job)

nameP :: Parser String
nameP = count 4 lowerChar

compP :: Parser Comp
compP = do
  a <- nameP
  _ <- space
  op <-
    choice
      [ Add <$ char '+',
        Sub <$ char '-',
        Mul <$ char '*',
        Div <$ char '/'
      ]
  _ <- space
  b <- nameP
  return (a, op, b)

eval :: Map String Int -> Map String Comp -> (Map String Int, Map String Comp)
eval nums comps =
  if nums == nums'
    then (nums', comps')
    else eval nums' comps'
  where
    (nums', comps') = step nums comps

step :: Map String Int -> Map String Comp -> (Map String Int, Map String Comp)
step nums comps = (nums', comps')
  where
    newNums = M.mapMaybe (compute nums) comps
    nums' = M.union nums newNums
    comps' = M.difference comps newNums

compute :: Map String Int -> Comp -> Maybe Int
compute nums (a, op, b) = case (ma, mb) of
  (Just x, Just y) -> Just $ applyOp x op y
  _ -> Nothing
  where
    ma = M.lookup a nums
    mb = M.lookup b nums

applyOp :: Int -> Op -> Int -> Int
applyOp x Add y = x + y
applyOp x Sub y = x - y
applyOp x Mul y = x * y
applyOp x Div y = x `div` y

invertComps :: Map String Int -> Map String Comp -> Map String Comp
invertComps nums = M.fromList . map (invert nums) . M.assocs

invert :: Map String Int -> (String, Comp) -> (String, Comp)
invert nums (x, (a, op, b)) = case (M.member a nums, op, M.member b nums) of
  (False, Add, True) -> (a, (x, Sub, b))
  (False, Sub, True) -> (a, (x, Add, b))
  (False, Mul, True) -> (a, (x, Div, b))
  (False, Div, True) -> (a, (x, Mul, b))
  (True, Add, False) -> (b, (x, Sub, a))
  (True, Sub, False) -> (b, (a, Sub, x))
  (True, Mul, False) -> (b, (x, Div, a))
  (True, Div, False) -> (b, (a, Div, x))
  _ -> error "unexpected comp"
