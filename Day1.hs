#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
-}

top3sums :: String -> [Int]
top3sums s = fst $ foldl update ([0, 0, 0], 0) (lines s)
  where
    update (as, v) "" = (top3 as v, 0)
      where
        top3 as@[a1, a2, a3] v
          | a1 < v = [v, a1, a2]
          | a2 < v = [a1, v, a2]
          | a3 < v = [a1, a2, v]
          | otherwise = as
    update (as, a) s = (as, (read s :: Int) + a)

main :: IO ()
main = do
  out <- readFile "data/01.txt"
  let topCalories = top3sums out
  print $ head topCalories
  print $ sum topCalories