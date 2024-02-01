#!/usr/bin/env runhaskell

import Text.Regex.PCRE
import Text.Printf(printf)
import Data.List(nubBy,foldl',intersect,sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Graph = M.Map String (Int, M.Map String Int) 

parse :: String -> (String, (Int, M.Map String Int))
parse l = (head ns, (r, M.fromList $ map (\x->(x,1)) $ drop 1 ns))
  where ns = getAllTextMatches $ l =~ "[A-Z]{2}" :: [String]
        r  = read (l =~ "[0-9]+" :: String) :: Int

contract :: Graph -> String -> Graph
contract ns0 k0 = case M.lookup k0 ns0 of
  Just (_,ups) -> M.mapWithKey (f ups) $ M.delete k0 ns0; _ -> ns0
  where f ups k (r,ns) = case M.lookup k0 ns of
          Just w -> (r, M.unionWith min (M.delete k0 ns) $ M.map (+w) $ M.delete k ups)
          _ -> (r,ns)

dfs :: Graph -> String -> [String] -> Int -> [(Int,[String])]
dfs ns0 k0 ks0 b | b < 2 = [] | otherwise = opens ++ moves
  where moves    = concatMap v1 $ poss ks0
        v1 (k,w) = dfs ns0 k (k0:ks0) (b-w)
        opens    = if r == 0 then [] else [(r*(b-1), [k0])] ++ concatMap v2 (poss [])
        v2 (k,w) = map (\(score, path) -> (score + r*(b-1), (k0):path)) $ dfs ns' k [] (b-w-1)
        (r, adj) = ns0 M.! k0
        ns'      = M.insert k0 (0, adj) ns0
        adj'     = filter (\(_,w)->b-w-2>0) $ M.toList adj
        poss ks' = filter (\(k,_)->not $ elem k ks') adj'

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  let ns  = M.fromList $ map parse $ lines str
      ns' = foldl' (\a x-> contract a x) ns [k | (k,(r,_))<-M.toList ns,r==0 && k/="AA"]
      ds  = nubBy (\(_,y1) (_,y2)->y1==y2) $ reverse $ sort $ S.toList $ S.fromList $ dfs ns' "AA" [] 26
      p1  = maximum $ map fst $ dfs ns' "AA" [] 30
      p2  = foldl' max 0 $ [i+j | (i,x)<-ds,(j,y)<-ds, i>=j && i+j>p1 && intersect x y==[]]
  printf "Part 1: %d, Part 2: %d\n" p1 p2
