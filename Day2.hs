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
score1 (l, r) = (r - l + 1) `mod` 3 * 3 + r

score2 :: (Int, Int) -> Int
score2 (l, r) = (l + r) `mod` 3 + 1 + (r - 1) * 3

main :: IO ()
main = do
  input <- readFile "data/02.txt"
  let games = parse input
  print . sum $ score1 <$> games
  print . sum $ score2 <$> games
