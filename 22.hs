#!/usr/bin/env runhaskell

import Text.Regex.PCRE
import Data.List(elemIndex)
import Text.Printf(printf)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe(fromJust)

oIndex :: (Int,Int) -> Int
oIndex o = fromJust $ elemIndex o os where os = [(0,1),(1,0),(0,-1),(-1,0)]

walk2D :: S.Set (Int, Int, Char) -> [Char] -> (Int, Int) -> (Int, Int) -> (Int, (Int, Int))
walk2D coo (x':xs) o@(i,j) l@(a,b)
  | elem x' "LR"             = walk2D coo xs ([(0,1),(1,0),(0,-1),(-1,0)] !! idx) l
  | S.member (i',j','.') coo = walk2D coo xs o (i',j')
  | otherwise                = walk2D coo xs o l
  where (i',j') = wrap
        idx = (oIndex o + (if x'=='R' then 1 else -1)) `mod` 4
        row = [x | (x,y,_) <- S.toList coo, y == b]
        col = [y | (x,y,_) <- S.toList coo, x == a]
        wrap | any (\x-> S.member (a+i,b+j,x) coo) ".#" = (a+i,b+j)
             | otherwise = [(a,minimum col),(minimum row,b),(a,maximum col),(maximum row,b)] !! oIndex (i,j)
walk2D _ [] o l = (oIndex o, l)

walk3D :: Int -> M.Map (Int, Int, Int) (a, b, Char) -> [Char] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
walk3D _ _ [] _ p = p
walk3D n map3D (x:xs) o path
  | elem x "LR" = walk3D n map3D xs next path
  | v == '.'    = walk3D n map3D xs o (path ++ [o])
  | otherwise   = walk3D n map3D xs o path
  where i'      = (oIndex o + (if x=='R' then 1 else -1)) `mod` 4
        next    = ([(0,1),(1,0),(0,-1),(-1,0)] !! i')
        p3D     = cubeWalk n $ path ++ [o]
        (_,_,v) = map3D M.! p3D

cubeWalk :: (Eq p, Num p) => p -> [(p, p)] -> (p, p, p)
cubeWalk n ins = cubeWalk' ins id (1,1,0)
  where cubeWalk' [] _ pos = pos
        cubeWalk' (o@(i',j'):xs) g (a',b',c')
          | (b'==n && b==n+1 && c==0) || (c'==n && b==n+1 && c==n+1) || (b'==1 && c==n+1 && b==0) || (c'==1 && b==0 && c==0)=
              cubeWalk' (o:xs) ((\(x,y,z)->(x,-z,y)) . g) (a,b,c)   -- (1|2),(2|3),(3|4),(4|1).
          | (b==n+1 && c==0) || (b==n+1 && c==n+1) || (c==n+1 && b==0) || (b==0 && c==0) =
              cubeWalk' (o:xs) ((\(x,y,z)->(x,z,-y)) . g) (a,b,c)   -- (1|4),(4|3),(3|2),(2|1)
          | (a'==1 && a==0 && c==0) || (c'==n && c==n+1 && a==0) || (c==n+1 && a'==n && a==n+1) || (a==n+1 && c'==1 && c==0) =
              cubeWalk' (o:xs) ((\(x,y,z)->(z,y,-x)) . g) (a,b,c)   -- (1|5),(5|3),(3|6),(6|1)
          | (a==0 && c==0) || (c==n+1 && a==0) || (c==n+1 && a==n+1) || (a==n+1 && c==0) =
              cubeWalk' (o:xs) ((\(x,y,z)->(-z,y,x)) . g) (a,b,c)   -- (1|6),(6|3),(3|5),(5|1)
          | (a'==1 && a==0 && b==n+1) || (b'==1 && b==0 && a==0) || (a==n+1 && a'==n && b==0) || (a==n+1 && b'==n && b==n+1) =
              cubeWalk' (o:xs) ((\(x,y,z)->(-y,x,z)) . g) (a,b,c)   -- (2|5),(5|4),(4|6),(6|2)
          | (a==0 && b==n+1) || (b==0 && a==0) || (a==n+1 && b==0) || (a==n+1 && b==n+1) =
              cubeWalk' (o:xs) ((\(x,y,z)->(y,-x,z)) . g) (a,b,c)   -- (2|6),(6|4),(4|5),(5|2)
          | otherwise = cubeWalk' xs g (a,b,c)
          where (i,j,k) = g (i', j', 0); (a,b,c) = (a'+i, b'+j, c'+k)

to3DMap :: (Num p, Ord p) => p -> M.Map (p, p) c -> [(p, p)] -> [([(p, p)], (p, p, c))] -> [((p, p, p), (p, p, c))]
to3DMap _ _ _ [] = []
to3DMap n coo vs ((p,ab@(a,b,_)):xs)
  | elem (a,b) vs = to3DMap n coo vs xs
  | otherwise = [(cubeWalk n p, ab)] ++ to3DMap n coo ((a,b):vs) (adj++xs)
  where poss = map (\(i,j)->(p++[(i,j)], (a+i,b+j))) [(0,1),(1,0),(0,-1),(-1,0)]
        adj  = map (\(x,y@(a',b'))->(x,(a',b', coo M.! y))) $ filter(\(_,x)-> M.member x coo && not (elem x vs)) poss

parse :: String -> (Int, [(Int, Int, Char)], [Char])
parse str = (n, coo, ins)
  where (str1,_,str2) = str =~ "(?s)\n\n" :: (String, String, String)
        ls   = lines str1
        n    = minimum $ map (length  . filter(/=' ')) $ ls
        coo  = filter (\(_,_,v) -> v /= ' ') $ [(i,j,v) | (i,l)<-zip [1..] ls, (j,v)<-zip [1..length l] l]
        ins  = concat $ map (\x->if x=~"[0-9]+" then replicate (read x) '.' else x) $ getAllTextMatches $ str2 =~ "L|R|[0-9]+"

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  let (n, coo, ins)  = parse str
      first          = ((\(a,b,_)->(a,b)) $ head coo)
      (face,(a',b')) = walk2D (S.fromList coo) ins (0,1) first
      part1          = 1000 * a' + 4 * b' + face
      coo'           = M.fromList $ zip (map (\(a,b,_)->(a,b)) coo) $ map (\(_,_,c)->c) coo
      to3D           = M.fromList $ to3DMap n coo' [] [([], head coo)]
      p2Path         = walk3D n to3D ins (0,1) []
      (a1,b1,_)      = to3D M.! (cubeWalk n p2Path)
      (a2,b2,_)      = to3D M.! (cubeWalk n $ init p2Path)
      part2          = 1000 *a1 + 4*b1 + (oIndex (a1-a2,b1-b2))
  printf "Part 1: %d, Part 2: %d TBD\n" part1 part2
