#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
-}

import Data.List

parse :: String -> [[Int]]
parse s = combine $ foldl parse' ([], []) (lines s)
  where
    combine (aas, as) = as : aas
    parse' acc "" = (combine acc, [])
    parse' (aas, as) s = (aas, (read s :: Int) : as)

combineCalories :: [[Int]] -> [Int]
combineCalories = fmap sum

max3 :: [Int] -> [Int]
max3 is = take 3 . reverse $ sort is

main :: IO ()
main = do
  out <- readFile "data/01.txt"
  let topCalories = max3 . combineCalories $ parse out
  print $ head topCalories
  print $ sum topCalories