#!/usr/bin/env runhaskell

import Data.List(elemIndex)
import Data.Char(ord)
import Text.Printf (printf)
import qualified Data.IntMap as M

main :: IO ()
main = do
  str <- getContents --readFile "sample.txt"
  let strip    = filter(/='\n') str
      grid     = M.fromList $ zip [1..] $ strip
      ei c     = maybe 0 id $ elemIndex c $ strip
      n        = maybe 0 id $ elemIndex '\n' str
      m        = (length str) `div` (n+1)
      ps       = [(x,y) | x<-[1..m], y<-[1..n]]
      val x | x=='S' = -(ord 'a') | x=='E' = -(ord 'z') | otherwise = -(ord x)
      q g f (x, y)  = maybe 1000 f $ M.lookup ((x-1)*n+y) g
      adj (x,y) = filter (\p->v0-1 <= q grid val p) opts
        where v0   = q grid val (x,y)
              opts = map (\(a,b)->(x+a,b+y)) [(1,0),(-1,0),(0,1),(0,-1)]
      start  = M.insert (1+ei 'E') 0 $ M.fromList $ zip [1..] $ replicate (n*m) 1000
      paths vs | vs' == vs = vs | otherwise = paths vs'
        where vs' = M.fromList $ zip [1..] $ map ff ps
              ff p = minimum $ q vs id p:(map ((+1).(q vs id)) $ adj p)
      solve = paths start
      part1 = M.lookup (1+ei 'S') $ solve
      part2 = minimum [x | (x,c) <- zip (M.elems solve) $ strip, elem c "Sa"]

  printf "Part 1: %s, Part 2: %d\n" (show part1) (part2 :: Int)
