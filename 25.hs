#!/usr/bin/env runhaskell

import Text.Printf(printf)
import qualified Data.Map.Strict as M

toInt :: String -> Int
toInt s = sum . map(\(a,b)->a*5^b) $ zip (map (\x->m M.! x) s) [n-1,n-2..0]
  where n = length s; m = M.fromList $ zip "210-=" [2,1,0,-1,-2]

toSnafu :: Int -> String
toSnafu n | n <= 2       = [m M.! n]
          | mod n 5 <= 2 = toSnafu (div n 5) ++ show (mod n 5)
          | otherwise    = toSnafu ((div n 5) + 1) ++ [m M.! (mod n (-5))]
  where m = M.fromList $ zip [2,1,0,-1,-2] "210-="

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  printf "Part 1: %s\n" (toSnafu . sum . map toInt $ lines str)
