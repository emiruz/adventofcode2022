#!/usr/bin/env runhaskell

import Text.Printf(printf)
import qualified Data.Set as S
import Data.Maybe (listToMaybe)

parse :: String -> S.Set (Int, Int)
parse str = S.fromList $ concat $ map pairs ls
  where ls = lines $ map (\x->if elem x "0123456789\n" then x else ' ') str
        pair (a:b:xs) = (a,b) : pair xs ; pair _ = []
        nums = map (\x->(read x)::Int) . filter (/="") . words
        pairs = concat . map (uncurry points) . (zip <*> tail) . pair . nums
        points (a,b) (c,d) | a == c    = [(a,y) | y <- [min b d .. max b d]]
                           | otherwise = [(x,b) | x <- [min a c .. max a c]]

sim :: (S.Set (Int,Int) -> Bool) -> S.Set (Int,Int) -> Int
sim f ds = length $ takeWhile f $ drop 1 $ iterate (evo (500,0)) ds
  where y' = maximum $ map snd $ S.toList ds
        evo p ds' = case mov ds' p of Just p'-> evo p' ds'; _-> S.insert p ds'
        cnd p@(_,y) d = S.member p d || y >= (y'+2)
        mov ds' (x,y) = listToMaybe [(x',y+1) | x'<-[x,x-1,x+1], not $ cnd (x',y+1) ds']

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  let ds0   = parse str
      y'    = maximum $ map snd $ S.toList ds0
      part1 = sim (\d-> y' >= (maximum $ map snd $ S.elems d)) ds0
      part2 = 1 + sim (\d-> not $ S.member (500,0) d) ds0
  printf "Part 1: %d, Part 2 %d:\n" part1 part2
