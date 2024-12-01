module Aoc.RangeSet where

import Data.Ix (range)
import Prelude
  ( Bool,
    Int,
    error,
    otherwise,
    sum,
    (&&),
    (+),
    (-),
    (.),
    (<),
    (<=),
    (>),
    (>=),
  )
import qualified Prelude as P

type Range = (Int, Int)

newtype RSet = RSet [Range]

fromRange :: Range -> RSet
fromRange r = RSet [r]

toList :: RSet -> [(Int, Int)]
toList (RSet rs) = rs

explode :: RSet -> [Int]
explode = P.concatMap range . toList

map :: (Range -> a) -> RSet -> [a]
map f = P.map f . toList

concatMap :: (Range -> [Range]) -> RSet -> RSet
concatMap f = RSet . P.concatMap f . toList

any :: (Range -> Bool) -> RSet -> Bool
any f = P.any f . toList

foldl :: (a -> Range -> a) -> a -> RSet -> a
foldl f a = P.foldl f a . toList

delete :: Range -> RSet -> RSet
delete (a, b) = concatMap rDelete
  where
    rDelete (s, e)
      | b < a = [(s, e)]
      | b < s = [(s, e)]
      | a > e = [(s, e)]
      | a <= s && b < e = [(b + 1, e)]
      | a <= s && b >= e = []
      | a > s && b < e = [(s, a - 1), (b + 1, e)]
      | a > s && b >= e = [(s, a - 1)]
      | otherwise = error "unreachable"

delete1 :: Int -> RSet -> RSet
delete1 i = delete (i, i)

member :: Int -> RSet -> Bool
member i = any rMember
  where
    rMember (s, e) = i >= s && i <= e

length :: RSet -> Int
length = sum . map rLength
  where
    rLength (s, e) = e - s + 1
