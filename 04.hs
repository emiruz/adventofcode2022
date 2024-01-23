#!/usr/bin/env runhaskell

import Data.List(intersect)
import Data.Char(isDigit)
import Text.Printf(printf)

subsume :: [Int] -> Int
subsume ns@[a,b,c,d] | (mn,mx) == (a,b) || (mn,mx) == (c,d) = 1
                     | otherwise = 0
  where (mn,mx) = (minimum ns, maximum ns)
subsume _ = 0

overlap :: [Int] -> Int
overlap [a,b,c,d] = length $ intersect [a..b] [c..d]
overlap _         = 0

main :: IO ()
main = do
  content <- getContents -- readFile "input.txt"
  let norm = map read . words . map (\x -> if isDigit x then x else ' ')
      ls   = map norm $ lines content
      s1   = sum $ map subsume ls
      s2   = length $ filter (>0) $ map overlap ls
  printf "Part 1: %d, Part 2: %d\n" s1 s2
