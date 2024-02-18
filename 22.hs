#!/usr/bin/env runhaskell

import Text.Regex.PCRE
import Data.List(elemIndex)
import Text.Printf(printf)
import qualified Data.Set as S
import Data.Maybe(fromJust)

type Coord = (Int, Int)
type Grid = S.Set (Int, Int, Char)

walk :: Grid -> String -> Coord -> Coord -> (Int, Coord)
walk coo (x':xs) o@(i,j) l@(a,b)
  | elem x' "LR"             = walk coo xs ([(0,1),(1,0),(0,-1),(-1,0)] !! idx) l
  | S.member (i',j','.') coo = walk coo xs o (i',j')
  | otherwise                = walk coo xs o l
  where (i',j') = wrap
        idx = (oIndex o + (if x'=='R' then 1 else -1)) `mod` 4
        row = [x | (x,y,_) <- S.toList coo, y == b]
        col = [y | (x,y,_) <- S.toList coo, x == a]
        wrap | any (\x-> S.member (a+i,b+j,x) coo) ".#" = (a+i,b+j)
             | otherwise = [(a,minimum col),(minimum row,b),(a,maximum col),(maximum row,b)] !! oIndex (i,j)
walk _ [] o l = (oIndex o, l)

oIndex :: Coord -> Int
oIndex o = fromJust $ elemIndex o os where os = [(0,1),(1,0),(0,-1),(-1,0)]

parse :: String -> ([(Int, Int, Char)], [Char])
parse str = (coo, ins)
  where (str1,_,str2) = str =~ "(?s)\n\n" :: (String, String, String)
        ls    = lines str1
        coo   = filter (\(_,_,v) -> v /= ' ') $ [(i,j,v) | (i,l)<-zip [1..] ls, (j,v)<-zip [1..length l] l]
        ins   = concat $ map (\x->if x=~"[0-9]+" then replicate (read x) '.' else x) $ getAllTextMatches $ str2 =~ "L|R|[0-9]+"

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  let (coo, ins)     = parse str
      first          = ((\(a,b,_)->(a,b)) $ head coo)
      (face,(a',b')) = walk (S.fromList coo) ins (0,1) first
      part1          = 1000 * a' + 4 * b' + face
  printf "Part 1: %d, Part 2: TBD\n" part1
