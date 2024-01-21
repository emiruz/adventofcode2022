#!/usr/bin/env runhaskell

import Data.List(elemIndex, intersect)
import Text.Printf(printf)

priority :: Char -> Int
priority x = maybe 0 (\i -> [1..] !! i) $ elemIndex x xs
  where xs = ['a'..'z'] ++ ['A'..'Z']

pick :: [String] -> [Int]
pick (x:y:z:xs) =
  (priority . head $ intersect x $ intersect y z) : pick xs
pick _ = []

splitIntersect :: String -> Int
splitIntersect xs =
  priority . head $ uncurry intersect $ splitAt (length xs `div` 2) xs

main :: IO ()
main = do
  content <- getContents -- readFile "input.txt"
  let ls = lines content
      s1 = sum $ map splitIntersect ls
      s2 = sum $ pick ls
  printf "Part 1: %d, Part 2: %d\n" s1 s2
