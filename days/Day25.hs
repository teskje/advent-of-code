module Main where

import Aoc (getInputLines)

main :: IO ()
main = do
  input <- getInputLines 25
  print $ (intToSnafu . sum . map snafuToInt) input

snafuToInt :: String -> Int
snafuToInt sns = sum $ map conv ins
  where
    ins = zip [0 ..] (reverse sns)
    conv :: (Int, Char) -> Int
    conv (i, sn) = snumToInt sn * (5 ^ i)

snumToInt :: Char -> Int
snumToInt '0' = 0
snumToInt '1' = 1
snumToInt '2' = 2
snumToInt '-' = -1
snumToInt '=' = -2
snumToInt _ = error "invalid snum"

intToSnafu :: Int -> String
intToSnafu 0 = ""
intToSnafu x = intToSnafu (x' + r) ++ [sn]
  where
    x' = x `div` 5
    (sn, r) = case x `mod` 5 of
      0 -> ('0', 0)
      1 -> ('1', 0)
      2 -> ('2', 0)
      3 -> ('=', 1)
      4 -> ('-', 1)
      _ -> error "invalid snum"

