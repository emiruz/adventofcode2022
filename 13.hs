#!/usr/bin/env runhaskell

import Text.Printf(printf)
import Data.List(sort,elemIndex)
import Data.Aeson
import qualified Data.Vector as V
import Data.ByteString.Lazy.Char8 (pack)

data P' = Lit Int | P [P'] deriving (Show,Eq)

instance FromJSON P' where
  parseJSON (Number n) = pure $ Lit $ round n
  parseJSON (Array a)  = P <$> mapM parseJSON (V.toList a)
  parseJSON _          = fail "Number or array expected."

instance Ord P' where
  compare (Lit x) (Lit y) = compare x y
  compare (P []) (P (_:_)) = LT
  compare (P (_:_)) (P []) = GT
  compare x@(Lit _) ys = compare (P [x]) ys
  compare xs y@(Lit _) = compare xs (P [y])
  compare (P (x:xs)) (P (y:ys))
    | compare x y == EQ = compare (P xs) (P ys)
    | otherwise = compare x y
  compare _ _ = EQ

parse :: String -> P'
parse x = maybe (P []) id $ decode $ pack x

main :: IO ()
main = do
  str <- getContents --readFile "input.txt"
  let ls = map parse $ filter (/="") $ lines str
      ps (a:b:xs) = (a,b) : ps xs; ps _ = []
      part1 = sum [i | (i,t)<- zip [1..] (map (\(a,b) -> a<b) $ ps ls), t]
      (start, end) = (parse "[[2]]", parse "[[6]]")
      ls' = sort $ (start:end:ls)
      part2 = product $ map (maybe 0 (+1) . flip elemIndex ls') [start,end]
  printf "Part 1: %d, Part 2 %d:\n" (part1 :: Int) part2
