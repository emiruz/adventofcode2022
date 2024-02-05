#!/usr/bin/env runhaskell

import Data.List (tails,isPrefixOf,findIndex)
import Text.Printf(printf)
import qualified Data.Set as S

type Coord = (Int,Int)

shapes :: Int -> Coord -> S.Set Coord
shapes n off = set off $ S.fromList $ case n `mod` 5 of
  0 -> [(0,0),(0,1),(0,2),(0,3)]       -- hline
  1 -> [(0,1),(1,0),(1,1),(1,2),(2,1)] -- cross
  2 -> [(0,0),(0,1),(0,2),(1,2),(2,2)] -- reverse L
  3 -> [(0,0),(1,0),(2,0),(3,0)]       -- vline
  _ -> [(0,0),(0,1),(1,0),(1,1)]       -- square

set :: Coord -> S.Set Coord -> S.Set Coord
set (x,y) = S.map (\(i,j)->(i+x, j+y))

simulate :: [Coord] -> S.Set Coord -> S.Set Coord -> Int -> [Int]
simulate xs'@(x:xs) state buff shp
  | buff == S.empty = simulate xs' state (shapes shp (maxX+4,2)) (shp+1)
  | x==(-1,0) && S.intersection next state /= S.empty = maxX : (simulate xs new S.empty shp)
  | S.intersection next state /= S.empty = simulate xs state buff shp
  | any (\(_,j)-> j < 0 || j > 6) next   = simulate xs state buff shp
  | otherwise = simulate xs state next shp
  where next      = set x buff
        new       = S.union state buff
        maxX      = maximum $ map fst $ S.toList state
simulate [] _ _ _ = []

main :: IO ()
main = do
  str <- getContents -- readFile "input.txt"
  let str'  = concat $ repeat $ filter (/='\n') str
      ins   = concatMap (\x-> [if x=='<' then (0,-1) else (0,1),(-1,0)]) str'
      sim   = simulate ins (S.fromList $ map (0,) [0..6]) S.empty 0
      diff  = zipWith (-) (tail sim) sim
      get i = (sum $ take i $ diff)
      n'    = 3000
      end   = take 50 $ drop n' $ diff
      first = maybe 0 id $ findIndex (isPrefixOf end) (tails diff)
      cyc = n' - first
      size  = (get n') - (get first)
      n     = 1000000000000 - first
      part2 = (n `div` cyc) * size + get (first + n `mod` cyc)
  printf "Part 1: %d Part 2: %d\n" (get 2022) part2

