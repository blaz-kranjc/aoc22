#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package containers
-}

import qualified Data.Set as Set

allUnique :: Ord a => [a] -> Bool
allUnique l = length l == Set.size (Set.fromList l)

indexNUnique :: Ord a => [a] -> Int -> Int
indexNUnique as n
  | allUnique (take n as) = n
  | otherwise = 1 + indexNUnique (tail as) n

main :: IO ()
main = do
  input <- readFile "data/06.txt"
  print $ indexNUnique input 4
  print $ indexNUnique input 14
