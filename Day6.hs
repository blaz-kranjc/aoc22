#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package containers
-}

import qualified Data.Set as Set

allUnique :: Ord a => [a] -> Bool
allUnique l = length l == Set.size (Set.fromList l)

indexNUnique :: Ord a => Int -> [a] -> Int
indexNUnique n as
  | allUnique . take n $ as = n
  | otherwise = 1 + indexNUnique n (tail as)

main :: IO ()
main = do
  input <- readFile "data/06.txt"
  print . indexNUnique 4 $ input
  print . indexNUnique 14 $ input
