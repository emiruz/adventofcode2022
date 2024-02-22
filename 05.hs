#!/usr/bin/env runhaskell

import Text.Regex.PCRE
import Data.List(transpose)
import Text.Printf(printf)

initStack :: String -> [String]
initStack ls = filter (/=[]) $ map norm $ transpose (lines ls)
  where norm xs = [x | x <- xs, elem x ['A'..'Z']]

move :: ([a] -> [a]) -> Int -> Int -> Int -> [[a]] -> [[a]]
move f0 n f t s = [ if i == f-1 then restF
                    else if i == t-1 then f0 movedF ++ restT
                    else x | (x, i) <- zip s [0..] ]
  where (movedF, restF) = splitAt n (s !! (f-1))
        restT = s !! (t-1)

main :: IO ()
main = do
  content <- getContents --readFile "input.txt"
  let (top,_,rest)      = content =~ "(?s)\n\n" :: (String,String,String)
      parseMove str     = map read (getAllTextMatches (str =~ "[0-9]+"))
      moves             = map parseMove (lines rest)
      move' f x [a,b,c] = move f a b c x
      move' _ x _       = x
      finalStack1       = foldl (move' reverse) (initStack top) moves
      finalStack2       = foldl (move' id) (initStack top) moves
      tops   s          = map head $ filter (/="") s
  printf "Part 1: %s, Part 2: %s\n" (tops finalStack1) (tops finalStack2)
