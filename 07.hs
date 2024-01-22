#!/usr/bin/env runhaskell

import Data.Char (isDigit)
import Text.Printf (printf)
import Data.List (sort, tails)
import Data.Map (fromListWith, toList)

unfold :: [String] -> [String] -> [([String], Int)]
unfold (x@(a:_):xs) ys
  | take 7 x == "$ cd .." = unfold xs (drop 1 ys)
  | take 4 x == "$ cd" = unfold xs ((drop 5 x):ys)
  | isDigit a = (ys, read . head $ words x) : (unfold xs ys)
  | otherwise = unfold xs ys
unfold _ _ = []

expand :: [([a], b)] -> [([a], b)]
expand xs = [(y, v) | (x, v) <- xs, y <- tails x, not (null y)]

group :: (Ord k, Num a) => [(k, a)] -> [(k, a)]
group xs = toList $ fromListWith (+) xs

main :: IO ()
main = do
  str <- getContents -- or readFile "input.txt"
  let sizes = sort . map snd . group . expand $ unfold (lines str) []
      smallDirs = sum . filter (<= 100000) $ sizes
      target = (maximum sizes) + 30000000 - 70000000
      targetSize = head $ filter (>= target) sizes
  printf "Part 1: %d, Part 2: %d\n" smallDirs targetSize
