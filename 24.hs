#!/usr/bin/env runhaskell

import Text.Printf(printf)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Heap as H
import Data.Maybe(fromJust)

evolve :: Int -> Int -> (Int,Int,(Int,Int)) -> (Int,Int,(Int,Int))
evolve m n (a,b,x@(i,j))
  | b' == 1   = (a',m-1,x) | b' == m = (a',2,x)
  | a' == 1   = (n-1,b',x) | a' == n = (2,b',x)
  | otherwise = (a',b',x)
  where (a',b') = (a+i,b+j)

bfs m n start (a',b') bliz seen heap
  | (a,b)==(a',b') = (bliz,t)
  | S.member (t,a,b) seen = bfs m n start (a',b') bliz seen heap_
  | otherwise      = bfs m n start (a',b') bliz' (S.insert (t,a,b) seen) heap'
  where heap' = foldl (\h p@(x,y) -> H.insert (score p, (t+1,x,y)) h) heap_ moves
        (xx@(_, (t,a,b)), heap_) = fromJust $ H.viewMin heap
        moves = filter (\p@(x,y)->p==start || p==(a',b') ||
                         ((not $ any (\(x',y',_)->(x,y)==(x',y')) bs) && x>1 && x<n && y>1 && y<m)) poss
        poss  = (a,b) : map (\(i,j)-> (a+i,b+j)) [(0,-1),(-1,0),(0,1),(1,0)]
        score (x,y) = t+1 + abs (a'-x) + abs (b'-y)
        (bs, bliz') = case M.lookup (t+1) bliz of
          Just bs -> (bs, bliz)
          Nothing -> let v = map (evolve m n) (bliz M.! t) in (v, M.insert (t+1) v bliz)

main :: IO ()
main = do
  str <- getContents -- readFile "input.txt"
  let o     = M.fromList [('<',(0,-1)), ('^',(-1,0)), ('>',(0,1)), ('v',(1,0))]
      ls    = lines str
      cs    = [(i,j,v) | (i,r)<-zip [1..] ls,(j,v)<-zip [1..] r]
      (m,n) = (length $ head ls, length ls) 
      (start@(x,y), end@(x',y')) = ((1,2), (n,m-1))
      b1    = map (\(a,b,v)-> (a,b,o M.! v)) $ filter (\(_,_,v)-> elem v "^<v>") cs
      (b2,part1) = bfs m n start end (M.fromList [(0,b1)]) S.empty $ H.singleton (0,(0,x,y))
      (b3,ret)  = bfs m n end start b2 S.empty $ H.singleton (0,(part1,x',y'))
      (_, part2)  = bfs m n start end b3 S.empty $ H.singleton (0,(ret,x,y))
  printf "Part 1: %d, Part 2: %d\n" part1 part2
