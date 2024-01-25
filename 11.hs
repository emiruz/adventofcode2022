#!/usr/bin/env runhaskell

import Text.Regex.PCRE
import Data.List (sort)
import Text.Printf (printf)
import Data.Map (fromListWith, toList)

parse :: Int -> String -> ([Int->(Int,Int)], [(Int,Int)])
parse mode str = (fs',qs)
  where regex r s = getAllTextMatches $ s =~ (r :: String) :: [String]
        mapInt    = map (\x -> read x :: Int)
        start     = map mapInt $ map (regex "[0-9]+") $ regex "(?<=items: ).+" str
        qs        = [(i,x) | (i,l) <- zip [0..] start, x<-l]
        divBy     = mapInt $ regex "(?<= by )[0-9]+" str
        ifTru     = mapInt $ regex "(?<=If true: throw to monkey )[0-9]+" str
        ifFal     = mapInt $ regex "(?<=If false: throw to monkey )[0-9]+" str
        test      = map (\(d, a, b) x -> if mod x d == 0 then (a,x) else (b,x)) (zip3 divBy ifTru ifFal)
        fs        = map (opF . words) $ regex "[\\*\\+].+" str
        prod      = product divBy
        f'        = if mode==0 then (\f x->div (f x) 3) else (\f x->mod (f x) prod)
        fs'       = zipWith (.) test $ map f' fs
        opF ["*","old"] = (\x -> x*x)
        opF ["*",n]     = (* (read n :: Int))
        opF ["+",n]     = (+ (read n :: Int))
        opF _           = id

step :: [(Int,Int)] -> (Int->(Int,Int)) -> Int -> ([(Int,Int)],[(Int,Int)])
step q f i = foldl (\(q1,q2) y@(j,x)->if i/=j then (q1++[y],q2) else (q1,q2++[f x])) ([],[]) q

rnds :: ([Int -> (Int, Int)], [(Int, Int)]) -> Int -> ([(Int, Int)], [(Int, Int)])
rnds (fs,qs) n = (iterate rnd (qs,[])) !! n
  where rnd (qs',cs')  = foldl f (qs',cs') (zip fs [0..])
        f (q,cs) (f',i) = let (q1,q2) = step q f' i in (q1++q2, (i, length q2) : cs)

main :: IO ()
main = do
  str <- getContents -- readFile "input.txt"
  let grp1 = toList $ fromListWith (+) $ snd $ rnds (parse 0 str) 20
      grp2 = toList $ fromListWith (+) $ snd $ rnds (parse 1 str) 10000
      cnt  = product . take 2 . reverse . sort . map snd
  printf "Part 1: %d, Part 2: %d\n" (cnt grp1) (cnt grp2)
