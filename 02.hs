#!/usr/bin/env runhaskell

import Data.List(elemIndex)
import Text.Printf(printf)

score :: String -> Int
score x = maybe 0 (+1) $ elemIndex x xs
  where xs = ["BX","CY","AZ","AX","BY","CZ","CX","AY","BZ"]

pick :: String -> String
pick x = maybe x (\i -> ys !! i) $ elemIndex x xs
  where xs = ["AX","AY","AZ","CX","CY","CZ"]
        ys = ["AZ","AX","AY","CY","CZ","CX"]

main :: IO ()
main = do
  content <- getContents --readFile "input.txt"
  let ls = map (filter (/=' ')) $ lines content
      s1 = sum $ map score ls
      s2 = sum $ map (score . pick) ls
  printf "Part 1: %d, Part 2: %d\n" s1 s2
