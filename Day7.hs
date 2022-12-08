#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package containers
-}

import qualified Data.Map as Map
import Data.List
import Data.Maybe

parse :: [String] -> [([String], Integer)]
parse = fst . foldl update ([], [])
  where
    update (filesystem, stack) output
      | output == "$ cd .." = (filesystem, tail stack)
      | output == "$ cd /" = (filesystem, [])
      | take 4 output == "$ cd" = (filesystem, drop 5 output:stack)
      | output == "$ ls" || take 3 output == "dir" = (filesystem, stack)
      | otherwise =
        let size = read . takeWhile (/=' ') $ output
        in ((stack, size):filesystem, stack)

sizes :: [([String], Integer)] -> [Integer]
sizes = fmap snd . Map.toList . foldl updateMap Map.empty
  where
    parts dirs = flip drop dirs <$> [0..(length dirs)]
    updateMap m (dirs, size) = foldl (updateSize size) m (parts dirs)
    updateSize size m dir = Map.alter (pure . (+size) . fromMaybe 0) dir m

main :: IO ()
main = do
  input <- readFile "data/07.txt"
  let filesystemSizes = sizes . parse . lines $ input
  print . sum . filter (<100000) $ filesystemSizes
  let available = 70000000 - maximum filesystemSizes
  print . minimum . filter (\s -> available + s >= 30000000) $ filesystemSizes
