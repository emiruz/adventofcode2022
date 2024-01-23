#!/usr/bin/env runhaskell

import Text.Printf (printf)
import Data.List(nub)

parse :: String -> [(Int,Int)]
parse str = map look (concat ins)
  where look c = maybe (0,1) id $ lookup c [('U',(-1,0)),('D',(1,0)),('L',(0,-1))]
        ins    = [replicate (read n) (head c) | [c,n] <- map words (lines str)]

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (a,b) (x,y)
  | (a-x)*(a-x) + (b-y)*(b-y) > 2 = (x+dx,y+dy)
  | otherwise = (x,y)
  where dx = signum (a-x) * min (abs (a-x)) 1
        dy = signum (b-y) * min (abs (b-y)) 1

trace :: [(Int,Int)] -> [(Int,Int)] -> [[(Int,Int)]]
trace ((a',b'):xs) ((a,b):t) =  new : trace xs (reverse new)
  where new = foldl (\acc y -> move (head acc) y : acc) [(a+a',b+b')] t
trace _ _ = []

main :: IO ()
main = do
  str <- getContents -- readFile "input.txt"
  let ins = parse str
  let cnt n = length $ nub [x | (x:_) <- trace ins (replicate n (0,0))]
  printf "Part 1: %d, Part 2: %d\n" (cnt 2) (cnt 10)
