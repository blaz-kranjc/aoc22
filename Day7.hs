#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package containers
  --package split
-}

import qualified Data.Map as Map
import Data.List
import Data.List.Split

data File = File String Integer

parse :: [String] -> [([String], File)]
parse s = fst $ foldl update ([], []) s
  where
    update acc@(filesystem, stack) output
      | output == "$ cd .." = (filesystem, tail stack)
      | take 4 output == "$ cd" = (filesystem, drop 5 output:stack)
      | output == "$ ls" || take 3 output == "dir" = acc
      | otherwise =
        let [size, name] = splitOn " " output
        in ((stack , File name (read size)):filesystem, stack)

sizes :: [([String], File)] -> [Integer]
sizes filesystem = snd <$> Map.toList (foldl updateMap Map.empty filesystem)
  where
    parts dirs = flip drop dirs <$> [0..(length dirs - 1)]
    updateMap m (dirs, File name size) = foldl (updateSize size) m (parts dirs)
    updateSize size m dir = Map.insert dir (Map.findWithDefault 0 dir m + size) m

main :: IO ()
main = do
  input <- readFile "data/07.txt"
  let filesystemSizes = sort . sizes . parse $ lines input
  print $ sum $ takeWhile (<100000) filesystemSizes
  let available = 70000000 - last filesystemSizes
  print $ head $ dropWhile (\s -> available + s < 30000000) filesystemSizes
