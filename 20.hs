#!/usr/bin/env runhaskell

import Data.Maybe (fromJust)
import Text.Printf(printf)
import qualified Data.Sequence as S

mix :: Eq a => S.Seq (a, b) -> (a, b) -> Int -> S.Seq (a, b)
mix sq x'@(x,_) o = move $ mod (o+i) (-1 + length sq)
  where i      = fromJust $ S.findIndexL ((==x).fst) sq
        move j = S.insertAt j x' (S.deleteAt i sq)

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  let ns = (zip [1..] (map read $ lines str)) :: [(Int,Int)]
      mix' sq x@(_,o)
        | o >= 0    = mix sq x o
        | otherwise = S.reverse $ mix (S.reverse sq) x (-o)
      go m y = sum $ map (\x->snd $ S.index mx' $ (x+zi') `mod` n) [1000,2000,3000]
        where ns' = map (\(a,b) -> (a,b*y)) ns
              mx' = foldl mix' (S.fromList $ ns') $ concat $ replicate m ns'
              n   = length mx'
              zi' = fromJust $ S.findIndexL ((==0) . snd) mx'
  printf "Part 1: %d, Part 2: %d\n" (go 1 1) (go 10 811589153)
