#!/usr/bin/env runhaskell

import Text.Printf(printf)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.Set as S

play :: (Num a, Num b, Ord a, Ord b) => S.Set (a, b) -> Int -> Int -> (Int, S.Set (a, b))
play es start end | start==end || uniq==[] = (start, es) | otherwise = play es' (start + 1) end
  where opts  = map S.fromList [[(-1,-1),(-1,0),(-1,1)],[(1,-1),(1,0),(1,1)],[(-1,-1),(0,-1),(1,-1)],[(-1,1),(0,1),(1,1)]]
        dirs   = foldl (\a x-> S.union a x) S.empty opts
        apply (a,b) = S.map (\(i,j)->(a+i,b+j))
        f (a,b)     = take 1 $ dropWhile (\x-> S.intersection x es /= S.empty) $
          map (\i->(apply (a,b) (opts !! i))) $ map (\x->mod x 4) [start..start+3]
        es_   = S.filter (\x-> S.intersection (apply x dirs) es /= S.empty) es
        moves = [(x, S.elemAt 1 $ head s) | (x,s)<-S.toList $ S.map (\x->(x, f x)) es_, s/=[]]
        uniq  = concat $ filter ((==1) . length) $ groupBy ((==) `on` snd) $ sortBy (comparing snd) moves
        es'   = foldl (\acc (a,b)->S.insert b (S.delete a acc)) es uniq

eval :: S.Set (Int, Int) -> Int
eval es = (1+mxX-mnX) * (1+mxY-mnY) - S.size es
  where (xs,ys)   = (S.map fst es, S.map snd es)
        (mnX,mxX) = (S.findMin xs, S.findMax xs)
        (mnY,mxY) = (S.findMin ys, S.findMax ys)

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  let es    = S.fromList [(i,j) | (i,r)<-zip [1..] (lines str),(j,v)<-zip [1..] r, v=='#']
  printf "Part 1: %d, Part 2: %d\n" (eval . snd $ play es 0 10) (1 + (fst $ play es 0 1000000))
