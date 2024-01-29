#!/usr/bin/env runhaskell

import Text.Regex.PCRE
import Text.Printf(printf)
import Data.Range

shapes :: [Int] -> [((Int,Int),Int)]
shapes ns = [((a,b), abs (a-c) + abs (b-d))| (a,b,c,d) <- quad ns]
  where quad (a:b:c:d:xs) = (a,b,c,d) : quad xs ; quad _ = []

nums :: String -> [Int]
nums str = map (\x->(read x) :: Int) strs
  where strs = getAllTextMatches $ str =~ "(?s)[\\-0-9]+" :: [String]

main :: IO ()
main = do
  str <- getContents -- readFile "input.txt"
  let (ns, ss) = (nums str, shapes ns)
      int y ((x0,y0),r) = let d=r-abs (y-y0) in
        if d <= 0 then [] else [(x0-d) +=+ (x0+d)]
      span' (SpanRange (Bound a _) (Bound b _)) = b-a; span' _ = 0
      spans y = mergeRanges $ concat $ map (int y) ss
      part1 = sum $ map span' $ spans 2000000
      y'   = length . takeWhile ((==1) . length) $ map spans $ [0..4000000]
      idx = filter (\x -> case x of SpanRange _ _->True;_->False) $ invert $ spans y'
      part2 = y' + 4000000 * (head $ fromRanges idx)
  printf "Part 1: %d, Part 2: %d\n" part1 part2
