#!/usr/bin/env runhaskell

import Text.Regex.PCRE
import Text.Printf(printf)
import qualified Data.Map.Strict as M

parse :: String -> [(Int,Int,Int,Int,Int,Int)]
parse str = bp $ map (\x->read x :: Int) strs
  where strs = getAllTextMatches $ str =~ "(?s)[0-9]+" :: [String]
        bp (_:a:b:c:d:e:f:xs) = (a,b,c,d,e,f) : bp xs; bp _ = []

opt :: (Integral b) => M.Map (Int, [b]) b -> [b] ->Int->(b,b,b,b,b,b) -> (M.Map (Int, [b]) b, b)
opt cache state 0 _ = (cache, last $ take 4 state)
opt cache state@[i,j,k,l,m,n,o,p] bu bp@(a,b,c,d,e,f) = case M.lookup (bu,state) cache of
  Just score' -> (cache, score')
  Nothing -> (M.insert (bu,state) val cache', val)
  where val = maximum next
        (cache',next) = foldl (\(ca,vs) x->let (c',v)=opt ca x (bu-1) bp in (c',v:vs)) (cache,[]) next'
        next' = map (zipWith (+) [m,n,o,p,0,0,0,0]) $  next''
        next'' | i>=e && k>=f = [geo] | i>=c && j>=d = [obs] | otherwise = filter (all (>=0)) $ next'''
        next''' = if cond then [nul,ore,cla] else [nul]
        cond = any (==0) [mod i a, mod i b, mod i c, mod i e, mod j d, mod k f]
        nul   = [i,   j,   k,   l, m,   n,   o,   p]
        ore   = [i-a, j,   k,   l, m+1, n,   o,   p]
        cla   = [i-b, j,   k,   l, m,   n+1, o,   p]
        obs   = [i-c, j-d, k,   l, m,   n,   o+1, p]
        geo   = [i-e, j,   k-f, l, m,   n,   o,   p+1]
opt _ _ _ _ = (M.empty,0)

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  let bps = parse str
      p1' = map (snd . opt M.empty [0,0,0,0,1,0,0,0] 24) bps
      p2' = map (snd . opt M.empty [0,0,0,0,1,0,0,0] 32) $ take 3 bps
  printf "Part 1: %d, Part 2: %d\n" (sum $ zipWith (*) [1..] p1') (product p2')
