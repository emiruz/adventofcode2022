#!/usr/bin/env runhaskell

import Text.Regex.PCRE
import Text.Printf(printf)
import Data.Ord (comparing)
import Data.List (minimumBy)
import qualified Data.Map.Strict as M

data Tree = Value Double | Node Double Tree Tree deriving (Show)

tree :: String -> M.Map String [String] -> Tree
tree n ms = case ms M.! n of
  [v]      -> Value (read v :: Double)
  [l,op,r] -> let (lt,rt) = (tree l ms, tree r ms)
              in Node (f' (ops M.! op) lt rt) lt rt
  _        -> Value 0
  where ops = M.fromList $ zip ["+","-","*","/"] [(+),(-),(*),(/)]
        f' f (Node x _ _) (Node y _ _) = f x y
        f' f (Value x) (Node y _ _)    = f x y
        f' f (Node x _ _) (Value y)    = f x y
        f' f (Value x) (Value y)       = f x y

eval :: Tree -> Double
eval t = let val (Value x) = x; val (Node x _ _) = x in val t

parse :: String -> M.Map String [String]
parse str = M.fromList $ f' $ map ps' (lines str)
  where f'    = map (\x-> (head x, drop 1 x))
        ps' l = getAllTextMatches $ l =~ "[0-9a-z+-/*/]+" :: [String]

bisect :: M.Map String [String] -> [(Double,Double)] -> Double
bisect rs xs
  | x == 0    = (a+b)/2
  | otherwise = bisect rs [(a,(a+b)/2), ((a+b)/2,b)]
  where ((a,b),x) = minimumBy (comparing snd) evals
        evals     = zip xs $ map (\(a',b')->eval' . next $ (a'+b')/2) xs
        next x'   = tree "root" $ M.insert "humn" [show x'] rs
        eval' (Node _ l r) = abs (eval l - eval r); eval' _ = -1

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  let rs      = parse str
      (p1,p2) = (eval $ tree "root" rs, bisect rs [(-999999999999999,999999999999999)])
  printf "Part 1: %.0f, Part 2: %.0f\n" p1 p2
