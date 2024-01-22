#!/usr/bin/env runhaskell

import Data.List(nub)
import Text.Printf(printf)

marker :: Int -> String -> Int
marker n (x:xs)
  | n == (length . nub $ take n (x:xs)) = n
  | otherwise = 1 + marker n xs
marker _ _ = 0

main :: IO ()
main = do
  xs <- getContents -- readFile "input.txt"
  printf "Part 1: %d\n, Part 2: %d\n" (marker 4 xs) (marker 14 xs)
