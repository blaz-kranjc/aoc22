#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package containers
  --package split
-}

import Data.Char
import Data.List.Split
import qualified Data.Set as Set

priority :: Char -> Int
priority c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1

splitHalf :: [a] -> ([a], [a])
splitHalf as = splitAt ((length as + 1) `div` 2) as

findCommon :: Ord a => [[a]] -> a
findCommon aas = Set.elemAt 0 $ foldl commons (Set.fromList $ head aas) aas
  where
    commons acc as = acc `Set.intersection` Set.fromList as

main :: IO ()
main = do
  input <- readFile "data/03.txt"
  let backpacks = lines input
  print $ sum $ priority . findCommon . (\(a, b) -> [a, b]) . splitHalf <$> backpacks
  print $ sum $ priority . findCommon <$> chunksOf 3 backpacks
