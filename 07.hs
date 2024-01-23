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

main :: IO ()
main = do
  str <- getContents -- or readFile "input.txt"
  let expand xs = [(y, v) | (x, v) <- xs, y <- tails x, not (null y)]
      group xs = toList $ fromListWith (+) xs
      sizes = sort . map snd . group . expand $ unfold (lines str) []
      smallDirs = sum . filter (<= 100000) $ sizes
      target = (maximum sizes) + 30000000 - 70000000
      targetSize = head $ filter (>= target) sizes
  printf "Part 1: %d, Part 2: %d\n" smallDirs targetSize
