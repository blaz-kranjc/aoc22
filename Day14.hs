#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package containers
  --package split
-}

import qualified Data.Set as S
import Data.List
import Data.List.Split

parsePoint :: String -> (Int, Int)
parsePoint s =
  let a = takeWhile (/= ',') s
      b = tail . dropWhile (/= ',') $ s
  in (read a, read b)

pointsBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pointsBetween (xa, ya) (xb, yb) =
  [(xa + signum (xb - xa) * n, ya + signum (yb - ya) * m) |
    n <- [0..abs (xb - xa)],
    m <- [0..abs (yb - ya)]]

pairs :: [a] -> [(a, a)]
pairs l = fmap (\l -> (l!!0, l!!1)) . take (length l - 1) . tails $ l

toPoints :: String -> S.Set (Int, Int)
toPoints = foldl1 S.union . fmap (S.fromList . uncurry pointsBetween) . pairs . fmap parsePoint . splitOn " -> "

simulateSand :: Int -> S.Set (Int, Int) -> (Int, Int) -> (Int, Int)
simulateSand floor obstacles p@(x, y)
  | y == floor = p
  | S.notMember (x, y+1) obstacles = simulateSand floor obstacles (x, y+1)
  | S.notMember (x-1, y+1) obstacles = simulateSand floor obstacles (x-1, y+1)
  | S.notMember (x+1, y+1) obstacles = simulateSand floor obstacles (x+1, y+1)
  | otherwise = p

simulateSandUntil :: ((Int, Int) -> Bool) -> Int -> S.Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
simulateSandUntil check floor obstacles origin =
  let
    go obs acc
      | check settled = acc
      | otherwise = go (S.insert settled obs) (settled:acc)
      where settled = simulateSand floor obs origin
  in go obstacles []

main :: IO ()
main = do
  input <- readFile "data/14.txt"
  let obstacles = foldl1 S.union . fmap toPoints . lines $ input
  let floor = (+1) . maximum . fmap snd . S.toList $ obstacles
  print . length $ simulateSandUntil ((==) floor . snd) floor obstacles (500, 0)
  print . (+1) . length $ simulateSandUntil ((500, 0) ==) floor obstacles (500, 0)
