#!/usr/bin/env runhaskell

import Data.List (sort)
import Text.Printf(printf)

sums :: [String] -> [Int]
sums = map sum . foldr get [[]]
  where  get "" (x:xs) = []:x:xs
         get y (x:xs)  = (read y:x):xs
         get _ []      = []

main :: IO ()
main = do
  content <- getContents --readFile "input.txt"
  let s = sums . lines $ content
  let top3 = negate . sum . take 3 . sort . map negate $ s
  printf "Part 1: %d, Part 2: %d\n" (maximum s) top3
