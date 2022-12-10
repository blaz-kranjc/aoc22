#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package containers
  --package split
-}

import Control.Monad
import Data.List.Split
import qualified Data.Set as Set

parse :: String -> [Int]
parse s
  | s == "noop" = [0]
  | otherwise = [0, read . drop 5 $ s]

display :: Set.Set Int -> IO ()
display s = void (mapM putStrLn . chunksOf 40 . fmap pixel $ [0..239])
  where
    pixel v
      | v `Set.member` s = '#'
      | otherwise = '.'

toPixels :: [Int] -> Set.Set Int
toPixels = Set.fromList . fmap fst . filter (\v -> abs ((fst v `mod` 40) - snd v) <= 1) . zip [0..]

main :: IO ()
main = do
  input <- readFile "data/10.txt"
  let xs = scanl (+) 1 . concatMap parse .lines $ input
  print xs
  let times = [20 + i * 40 | i <- [0..5]]
  print . sum . fmap (\t -> t * xs!!(t-1)) $ times
  let pixels = toPixels xs
  display pixels
