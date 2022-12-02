#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
-}

top3sums :: String -> [Int]
top3sums s = uncurry tops $ foldl update ([0, 0, 0], 0) $ lines s
  where
    tops [] _ = []
    tops a@(h:t) v
      | h < v = v : take (length t) a
      | otherwise = h : tops t v
    update (as, v) "" = (tops as v, 0)
    update (as, v) s = (as, v + read s :: Int)

main :: IO ()
main = do
  input <- readFile "data/01.txt"
  let topCalories = top3sums input
  print $ head topCalories
  print $ sum topCalories
