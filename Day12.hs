#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package vector
  --package search-algorithms
-}

import Data.Char
import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Algorithm.Search

data Matrix a = Matrix {
  entries :: Vector a,
  nRows :: Int,
  nColumns :: Int
}

parse :: String -> Matrix Char
parse s = Matrix { entries = out , nRows = length rows, nColumns = length . head $ rows }
  where
    rows = lines s
    out = V.fromList . concat $ rows

readAt :: Matrix a -> (Int, Int) -> Maybe a
readAt m (x, y)
  | x < 0 || y < 0 || x > nColumns m - 1 || y > nRows m - 1= Nothing
  | otherwise = Just (entries m ! (x + nColumns m * y))

find :: Eq a => Matrix a -> a -> Maybe (Int, Int)
find m v =
  let pos i = (i `mod` nColumns m, i `div` nColumns m)
  in fmap pos . V.elemIndex v . entries $ m

minSteps :: Matrix Char -> (Char -> Char -> Bool) -> Char -> Char -> Maybe Int
minSteps m validNeighbor from to = do
  start <- find m from
  fst <$> dijkstra neighbors cost target start
  where
    neighbors p@(x, y) =
      let isValid other = or $ fmap (validNeighbor (fromJust . readAt m $ p)) . readAt m $ other
      in filter isValid [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    cost _ _ = 1
    target = (== to) . fromJust . readAt m

validUp :: Char -> Char -> Bool
validUp f t
  | f == 'S' = t == 'a'
  | t == 'E' = f == 'z'
  | otherwise = ord t - ord f <= 1

validDown :: Char -> Char -> Bool
validDown f t
  | f == 'E' = t == 'z'
  | t == 'S' = f == 'a'
  | otherwise = ord f - ord t <= 1

main :: IO ()
main = do
  input <- readFile "data/12.txt"
  let m = parse input
  print . fromMaybe (-1) $ minSteps m validUp 'S' 'E'
  print . fromMaybe (-1) $ minSteps m validDown 'E' 'a'
