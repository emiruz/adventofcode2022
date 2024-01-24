#!/usr/bin/env runhaskell

import Text.Printf (printf)

parse :: [String] -> [(Int,Int)]
parse ls = zip (scanl (+) 1 [x | (x,_) <- ws]) (scanl (+) 1 [y | (_,y) <- ws])
  where ws = map (p' . words) ls
        p' ["addx",n] = (2,read n :: Int)
        p' _          = (1,0)

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  let cmds  = parse (lines str)
      val n = snd $ last $ takeWhile ((<= n) . fst) cmds
      mx    = maximum $ map fst cmds
      part1 = sum [j * val j | j <- takeWhile (<= mx) [20,60..]]
      chk v = abs (mod (v-1) 40 - (val v)) <= 1
      xs    = map (\i-> if chk i then '#' else '.') [1..mx]
      grp   = takeWhile (/=[]) $ map (take 40) $ iterate (drop 40) xs
  printf "Part 1: %s, Part 2:\n\n" (show $ part1)
  mapM_ putStrLn $ grp
