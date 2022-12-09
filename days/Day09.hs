module Main where

import Aoc (Parser, getParsedLines)
import Data.Set as S
import Text.Megaparsec (choice)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

main :: IO ()
main = do
  input <- getParsedLines 9 motion
  let moves = concat input
  print $ (length . S.fromList . dragTail 1) moves
  print $ (length . S.fromList . dragTail 9) moves

data Move = L | R | U | D
  deriving (Show)

motion :: Parser [Move]
motion = do
  d <- choice [L <$ char 'L', R <$ char 'R', U <$ char 'U', D <$ char 'D']
  _ <- char ' '
  n <- decimal
  return (replicate n d)

type Pos = (Int, Int)

dragTail :: Int -> [Move] -> [Pos]
dragTail n = dragTail' (0, 0) (replicate n (0, 0))

dragTail' :: Pos -> [Pos] -> [Move] -> [Pos]
dragTail' _ _ [] = []
dragTail' h t (m : ms) = last t' : dragTail' h' t' ms
  where
    h' = move m h
    t' = follow h' t

move :: Move -> Pos -> Pos
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)

follow :: Pos -> [Pos] -> [Pos]
follow _ [] = []
follow h (t : ts) = t' : follow t' ts
  where
    t' = follow1 h t

follow1 :: Pos -> Pos -> Pos
follow1 (xh, yh) (xt, yt) = case (xh - xt, yh - yt) of
  (-2, dy) -> (xt - 1, yt + signum dy)
  (2, dy) -> (xt + 1, yt + signum dy)
  (dx, -2) -> (xt + signum dx, yt - 1)
  (dx, 2) -> (xt + signum dx, yt + 1)
  _ -> (xt, yt)
