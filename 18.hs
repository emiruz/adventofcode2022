#!/usr/bin/env runhaskell

import Text.Regex.PCRE
import Text.Printf(printf)
import qualified Data.Set as S

type Coord = (Int,Int,Int)

adj :: S.Set Coord -> Coord -> S.Set Coord
adj xs (x,y,z) = S.intersection xs test
  where test  = S.fromList $ map (\(i,j,k)->(x+i,y+j,z+k)) $ sides
        sides = [(0,0,1),(0,0,-1),(0,1,0),(0,-1,0),(1,0,0),(-1,0,0)]

parse :: String -> S.Set Coord
parse str = S.fromList $ triples nums
  where triples (a:b:c:xs) = (a,b,c) : triples xs ; triples _ = []
        digits = getAllTextMatches $ str =~ "(?s)[0-9]+" :: [String]
        nums   = map (\x -> read x :: Int) digits

fill :: S.Set Coord -> [Coord] -> S.Set Coord -> S.Set Coord 
fill cs (x:xs) ms
  | S.member x ms = fill cs xs ms
  | otherwise = fill cs ((S.toList $ adj cs x) ++ xs) (S.insert x ms)
fill _ [] ms = ms

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  let cs = parse str
      lim = (+1) $ S.findMax $ S.map (\(x,y,z) -> maximum [x,y,z]) cs
      cands  = S.fromList [(i,j,k) | i<-[-1..lim],j<-[-1..lim],k<-[-1..lim], not $ S.member (i,j,k) cs]
      cands' = S.difference cands $ fill cands [(-1,-1,-1)] S.empty
      part1  = sum $ map ((6-) . S.size . adj cs) $ S.toList cs
      part2  = sum $ map (S.size . adj cands') $ S.toList cs
  printf "Part 1: %d Part 2: %d\n" part1 (part1 - part2)
