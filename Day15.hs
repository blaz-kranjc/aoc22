#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package range-set-list
-}

import Data.RangeSet.List as RS

parseData :: String -> ((Int, Int), (Int, Int))
parseData s =
  let (sx, r) = span (/=',') (drop 12 s)
      (sy, r') = span (/=':') (drop 4 r)
      (bx, r'') = span (/=',') (drop 25 r')
      by = drop 4 r''
  in ((read sx, read sy), (read bx, read by))

findExclusionsYPos :: [((Int, Int), (Int, Int))] -> Int -> RS.RSet Int
findExclusionsYPos readings yEx =
  let exclusion (x, y) (x', y') = (x, abs (x - x') + abs (y - y') - abs (y - yEx))
      exclusionZones = fmap (\(x, d) -> (x - d, x + d)) . filter ((>= 0) . snd) . fmap (uncurry exclusion) $ readings
  in foldl (flip RS.insertRange) RS.empty exclusionZones


filterBeacons :: [((Int, Int), (Int, Int))] -> Int -> RS.RSet Int -> RS.RSet Int
filterBeacons readings yEx exclusions =
  let beaconsAtPos = fmap fst . filter ((==) yEx . snd) . fmap snd $ readings
  in foldl (flip RS.delete) exclusions beaconsAtPos

findBeacon :: Int -> [((Int, Int), (Int, Int))] -> (Int, Int)
findBeacon distance readings =
  let getBlinds = RS.difference (RS.singletonRange (0, distance))
      blindSpots y = getBlinds $ findExclusionsYPos readings y
      toPoint y rs = (findMin rs, y)
  in  uncurry toPoint . head . filter (not . RS.null . snd) $ (\y -> (y, blindSpots y)) <$> [0..distance]

tunningFrequency :: Int -> (Int, Int) -> Integer
tunningFrequency distance (x, y) = toInteger x * toInteger distance + toInteger y

main :: IO ()
main = do
  input <- readFile "data/15.txt"
  let readings = parseData <$> lines input
  print . RS.size . filterBeacons readings 2000000 $ findExclusionsYPos readings 2000000
  print . tunningFrequency 4000000 $ findBeacon 4000000 readings
