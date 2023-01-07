#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package split
  --package containers
-}

import Data.List
import Data.List.Split
import qualified Data.Set as S

parse :: String -> (Int, Int, Int)
parse s =
  let [x, y, z] = splitOn "," s
  in (read x, read y, read z)

neighbours :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbours (x, y, z) =
  [ (x+1, y, z) , (x-1, y, z)
  , (x, y+1, z) , (x, y-1, z)
  , (x, y, z+1) , (x, y, z-1)]

area :: S.Set (Int, Int, Int) -> Int
area pixels =
  let countOpen = length . filter (`S.notMember` pixels) . neighbours
  in sum $ countOpen <$> S.toList pixels

outsideArea :: S.Set (Int, Int, Int) -> Int
outsideArea pixels =
  let pixelsList = S.toList pixels
      (x0, y0, z0) = head pixelsList
      updateBB ((xm, xM), (ym, yM), (zm, zM)) (x, y, z) = (
        (min xm (x-1), max xM (x+1)),
        (min ym (y-1), max yM (y+1)),
        (min zm (z-1), max zM (z+1)))
      ((xm, xM), (ym, yM), (zm, zM)) = foldl updateBB ((x0-1, x0+1), (y0-1, y0+1), (z0-1, z0+1)) (tail pixelsList)
      inBB (x, y, z) = x >= xm && x <= xM && y >= ym && y <= yM && z >= zm && z <= zM
      countOutside visited vs n
        | S.null vs = n
        | otherwise =
          let ns = filter (`S.notMember` visited) . concatMap (filter inBB . neighbours) . S.toList $ vs
              (borders, rests) = partition (`S.member` pixels) ns
          in countOutside (S.union visited vs) (S.fromList rests) (n+length borders)
  in countOutside S.empty (S.fromList [(xm, ym, zm)]) 0

main :: IO ()
main = do
  input <- readFile "data/18.txt"
  let pixels = S.fromList . fmap parse . lines $ input
  print . area $ pixels
  print . outsideArea $ pixels
