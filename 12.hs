#!/usr/bin/env runhaskell

import Data.List(elemIndex,foldl')
import Data.Char(ord)
import Text.Printf (printf)
import qualified Data.IntMap as M

main :: IO ()
main = do
  str <- getContents --readFile "sample.txt"
  let strip = filter(/='\n') str
      grid  = M.fromList $ zip [1..] $ strip
      ei c  = maybe 0 id $ elemIndex c $ strip
      (n,m) = (maybe 0 id $ elemIndex '\n' str, (length str) `div` (n+1))
      ps    = [(x,y) | x<-[1..m], y<-[1..n]]

      val x | x=='S' = -(ord 'a') | x=='E' = -(ord 'z') | otherwise = -(ord x)

      q g f (x,y)  = maybe 1000 f $ M.lookup ((x-1)*n+y) g

      adj (x,y)    = filter (\p->v0-1 <= q grid val p) opts
        where v0   = q grid val (x,y)
              opts = map (\(a,b)->(x+a,b+y)) [(1,0),(-1,0),(0,1),(0,-1)]

      adj'  = zip [1..] (map adj ps)
      start = M.insert (1+ei 'E') 0 $ M.fromList $ zip [1..] $ replicate (n*m) 1000

      paths vs | chg==0 = vs | otherwise = paths vs'
        where (chg, vs') = foldl' (\acc y@(v0,(i,_))-> upd acc i (ff y) v0) (0,vs) (zip (M.elems vs) adj')
              upd (chg, acc) i v v0 = if v /= v0 then (1, M.insert i v acc) else (chg, acc)
              ff (v0,(_,a)) = minimum $ v0:(map ((+1).(q vs id)) a)

      solve = paths start
      part1 = maybe 0 id $ M.lookup (1+ei 'S') solve
      part2 = minimum [x | (x,c) <- zip (M.elems solve) $ strip, elem c "Sa"]

  printf "Part 1: %d, Part 2: %d\n" part1 (part2 :: Int)
