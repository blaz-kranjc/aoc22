#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package split
-}

import Data.List.Split

parse :: String -> ((Int, Int), (Int, Int))
parse s =
  let [a, b, c, d] = splitOneOf ",-" s
  in ((read a, read b), (read c, read d))

fullyOverlaps :: (Int, Int) -> (Int, Int) -> Bool
fullyOverlaps (a, b) (c, d) = ((a <= c) && (b >= d)) || ((c <= a) && (d >= b))

overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (a, b) (c, d) = ((c >= a) && (c <= b)) || ((a >= c) && (a <= d))

main :: IO ()
main = do
  input <- readFile "data/04.txt"
  let ranges = fmap parse . lines $ input
  print . length . filter (uncurry fullyOverlaps) $ ranges
  print . length . filter (uncurry overlaps) $ ranges
