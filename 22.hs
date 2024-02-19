#!/usr/bin/env runhaskell

import Text.Regex.PCRE
import Data.List(elemIndex)
import Text.Printf(printf)
import qualified Data.Map.Strict as M
import Data.Maybe(fromJust)

oIndex :: (Int,Int) -> Int
oIndex o = fromJust $ elemIndex o os where os = [(0,1),(1,0),(0,-1),(-1,0)]

walk2D :: M.Map (Int, Int) Char -> [Char] -> (Int, Int) -> (Int, Int) -> (Int, (Int, Int))
walk2D _ [] o l = (oIndex o, l)
walk2D coo (x':xs) o@(i,j) l@(a,b)
  | elem x' "LR"           = walk2D coo xs ([(0,1),(1,0),(0,-1),(-1,0)] !! idx) l
  | coo M.! (i',j') == '.' = walk2D coo xs o (i',j')
  | otherwise              = walk2D coo xs o l
  where (i',j') = wrap
        idx = (oIndex o + (if x'=='R' then 1 else -1)) `mod` 4
        row = [x | (x,y) <- M.keys coo, y == b]
        col = [y | (x,y) <- M.keys coo, x == a]
        wrap | M.member (a+i,b+j) coo = (a+i,b+j)
             | otherwise = [(a,minimum col),(minimum row,b),(a,maximum col),(maximum row,b)] !! oIndex (i,j)

walk3D _ _ [] _ _ _ = []
walk3D n map3D (x:xs) o g p3D
  | elem x "LR" = walk3D n map3D xs next g p3D
  | v == '.'    = p3D' : walk3D n map3D xs o g' p3D'
  | otherwise   = walk3D n map3D xs o g p3D
  where i'        = (oIndex o + (if x=='R' then 1 else -1)) `mod` 4
        next      = ([(0,1),(1,0),(0,-1),(-1,0)] !! i')
        (g',p3D') = cubeStep n o g p3D
        (_,_,v)   = map3D M.! p3D'

cubeStep n o@(i',j') g (a',b',c')
  | (b'==n && b==n+1 && c==0) || (c'==n && b==n+1 && c==n+1) || (b'==1 && c==n+1 && b==0) || (c'==1 && b==0 && c==0)=
      cubeStep n o ((\(x,y,z)->(x,-z,y)) . g) (a,b,c)   -- (1|2),(2|3),(3|4),(4|1).
  | (b==n+1 && c==0) || (b==n+1 && c==n+1) || (c==n+1 && b==0) || (b==0 && c==0) =
      cubeStep n o ((\(x,y,z)->(x,z,-y)) . g) (a,b,c)   -- (1|4),(4|3),(3|2),(2|1)
  | (a'==1 && a==0 && c==0) || (c'==n && c==n+1 && a==0) || (c==n+1 && a'==n && a==n+1) || (a==n+1 && c'==1 && c==0) =
      cubeStep n o ((\(x,y,z)->(z,y,-x)) . g) (a,b,c)   -- (1|5),(5|3),(3|6),(6|1)
  | (a==0 && c==0) || (c==n+1 && a==0) || (c==n+1 && a==n+1) || (a==n+1 && c==0) =
      cubeStep n o ((\(x,y,z)->(-z,y,x)) . g) (a,b,c)   -- (1|6),(6|3),(3|5),(5|1)
  | (a'==1 && a==0 && b==n+1) || (b'==1 && b==0 && a==0) || (a==n+1 && a'==n && b==0) || (a==n+1 && b'==n && b==n+1) =
      cubeStep n o ((\(x,y,z)->(-y,x,z)) . g) (a,b,c)   -- (2|5),(5|4),(4|6),(6|2)
  | (a==0 && b==n+1) || (b==0 && a==0) || (a==n+1 && b==0) || (a==n+1 && b==n+1) =
      cubeStep n o ((\(x,y,z)->(y,-x,z)) . g) (a,b,c)   -- (2|6),(6|4),(4|5),(5|2)
  | otherwise = (g, (a,b,c))
  where (i,j,k) = g (i', j', 0); (a,b,c) = (a'+i, b'+j, c'+k)

to3DMap _ _ _ [] = []
to3DMap n coo vs (((g,p3D),ab@(a,b,_)):xs)
  | elem (a,b) vs = to3DMap n coo vs xs
  | otherwise = [(p3D,ab)] ++ to3DMap n coo ((a,b):vs) (adj++xs)
  where poss = map (\(i,j)->(cubeStep n (i,j) g p3D, (a+i,b+j))) [(0,1),(1,0),(0,-1),(-1,0)]
        adj  = map (\(x,y@(a',b'))->(x, (a',b', coo M.! y))) $ filter(\(_,x)-> M.member x coo && not (elem x vs)) poss

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
      coo'           = M.fromList $ zip (map (\(a,b,_)->(a,b)) coo) $ map (\(_,_,c)->c) coo      
      (face,(a',b')) = walk2D coo' ins (0,1) first
      part1          = 1000 * a' + 4 * b' + face
      to3D           = M.fromList $ to3DMap n coo' [] [((id,(1,1,0)), head coo)]
      ps             = map (\x->to3D M.! x) $ take 2 $ reverse $ walk3D n to3D ins (0,1) id (1,1,0)
      ((x,y,_),(x',y',_)) = (head ps, head $ drop 1 ps)
      part2          = 1000*x + 4*y + oIndex (x-x',y-y')
  printf "Part 1: %d, Part 2: %d\n" part1 part2
