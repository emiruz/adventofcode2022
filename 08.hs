#!/usr/bin/env runhaskell

{-# OPTIONS_GHC -Wall #-}

import Data.List(nub,transpose)
import Data.Char(digitToInt)
import Text.Printf (printf)
import Data.Map (fromListWith, toList)

grid :: String -> [[((Int,Int), Char)]]
grid s = zipWith (\l n -> zip [(n,x)| x <- [1..]] l) (lines s) [1..]

viz :: [(a, Char)] -> Int -> [(a, Char)]
viz ((a,b):xs) mx
  | (digitToInt b) > mx = (a,b) : viz xs (max (digitToInt b) mx)
  | otherwise = viz xs mx
viz _ _ = []

view :: Ord b => [(a, b)] -> [(a, Int)]
view ((a,b):xs) = (a,cnt') : view xs
  where cnt  = length $ takeWhile (\x -> b > snd x) xs
        cnt' = if cnt < length xs then cnt+1 else cnt
view _ = []

main :: IO ()
main = do
  str <- readFile "input.txt"
  let coo      = grid str
      viz' x   = nub $ (viz x (-1)) ++ (viz (reverse x) (-1))
      view' x  = view x ++ view (reverse x)
      cnt f    = foldl (\a x -> a ++ f x) []
      cnts     = length . nub $ cnt viz' coo ++ cnt viz' (transpose coo)
      group xs = toList $ fromListWith (*) xs
      groups   = group $ cnt view' coo ++ cnt view' (transpose coo)
  printf "Part 1: %d, Part 2: %d\n" cnts (maximum $ map snd groups)
