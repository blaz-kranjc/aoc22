#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
-}

import Data.Char

parse :: String -> [(Int, Int)]
parse s = parse1 <$> lines s
  where
    parseChar rock value = ord value - ord rock + 1
    parse1 l = (parseChar 'A' (l!!0), parseChar 'X' (l!!2))

score1 :: (Int, Int) -> Int
score1 (l, r) = score' result + r
  where
    result = (r - l + 3) `mod` 3
    score' 0 = 3
    score' 1 = 6
    score' _ = 0

score2 :: (Int, Int) -> Int
score2 (l, r) = result
  where
    result
      | r == 1 = [3, 1, 2]!!(l-1)
      | r == 2 = l + 3
      | r == 3 = [2, 3, 1]!!(l-1) + 6
      | otherwise = error "unexpected number"

main :: IO ()
main = do
  input <- readFile "data/02.txt"
  let games = parse input
  print . sum $ score1 <$> games
  print . sum $ score2 <$> games
