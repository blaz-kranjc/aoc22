#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package split
-}

import Data.List.Split

parse :: String -> [Int]
parse "noop" = [0]
parse s = [0, read . drop 5 $ s]

pixel :: (Int, Int) -> Char
pixel (i, v)
  | abs (i `mod` 40 - v) <= 1 = '#'
  | otherwise = '.'

main :: IO ()
main = do
  input <- readFile "data/10.txt"
  let xs = zip [0..] . scanl (+) 1 . concatMap parse .lines $ input
  let times = flip (-) 1 <$> [20 + i * 40 | i <- [0..5]]
  print . sum . fmap ((\v -> (fst v + 1) * snd v) . (xs!!)) $ times
  mapM_ putStrLn . chunksOf 40 . fmap pixel . take 240 $ xs
