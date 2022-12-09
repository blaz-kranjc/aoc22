#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package containers
-}

import qualified Data.Set as Set

data Direction = R | L | U | D deriving(Show)

parse :: String -> (Direction, Int)
parse s
  | head s == 'R' = (R, read . drop 2 $ s)
  | head s == 'L' = (L, read . drop 2 $ s)
  | head s == 'U' = (U, read . drop 2 $ s)
  | otherwise = (D, read . drop 2 $ s)

headPath :: [(Direction, Int)] -> [(Int, Int)]
headPath = reverse . fst . foldl update ([(0, 0)], (0, 0))
  where
    update a (direction, 0) = a
    update (visited, (x, y)) (d@R, amount) = update ((x+1, y):visited, (x+1, y)) (d, amount - 1)
    update (visited, (x, y)) (d@L, amount) = update ((x-1, y):visited, (x-1, y)) (d, amount - 1)
    update (visited, (x, y)) (d@U, amount) = update ((x, y+1):visited, (x, y+1)) (d, amount - 1)
    update (visited, (x, y)) (d@D, amount) = update ((x, y-1):visited, (x, y-1)) (d, amount - 1)

tailVisited :: [(Int, Int)] -> [(Int, Int)]
tailVisited = reverse . foldl update []
  where
    update [] v = [v]
    update pos@((x', y'):_) (x, y)
      | abs (x-x') <= 1 && abs (y-y') <= 1 = pos
      | x == x' = (x', y' + signum (y-y')):pos
      | y == y' = (x' + signum (x-x'), y'):pos
      | otherwise = (x' + signum (x-x'), y' + signum (y-y')):pos


main :: IO ()
main = do
  input <- readFile "data/09.txt"
  let headMotion = headPath . fmap parse . lines $ input
  print . length . Set.fromList . tailVisited $ headMotion
  print . length . Set.fromList . flip (!!) 9 . iterate tailVisited $ headMotion