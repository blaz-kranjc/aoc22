#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
-}

maxN :: Int -> [Int] -> [Int]
maxN n = foldl maxN' (replicate n 0)
  where
    maxN' [] _ = []
    maxN' a@(h:t) v
      | h < v = v : take (length t) a
      | otherwise = h : maxN' t v

parse :: String -> [[Int]]
parse s = combine $ foldl parse' ([], []) (lines s)
  where
    combine = uncurry $ flip (:)
    parse' acc "" = (combine acc, [])
    parse' (aas, as) s = (aas, (read s :: Int) : as)

main :: IO ()
main = do
  input <- readFile "data/01.txt"
  let topCalories = maxN 3 $ sum <$> parse input
  print $ head topCalories
  print $ sum topCalories
