#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
-}

import Data.Char

priority :: Char -> Int
priority c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1

duplicated :: Eq a => [a] -> [a] -> a
duplicated _ [] = error "No duplicates"
duplicated as (b:bs)
  | b `elem` as = b
  | otherwise = duplicated as bs

splitHalf :: [a] -> ([a], [a])
splitHalf as = splitAt ((length as + 1) `div` 2) as

duplicated' :: Eq a => ([a], [a], [a]) -> a
duplicated' (_, _, []) = error "No duplicates"
duplicated' (as, bs, c:cs)
  | c `elem` as && c `elem` bs = c
  | otherwise = duplicated' (as, bs, cs)

chunk3 :: [a] -> [(a, a, a)]
chunk3 [] = []
chunk3 as = (a, b, c) : chunk3 cs
    where ([a, b, c], cs) = splitAt 3 as

main :: IO ()
main = do
  input <- readFile "data/03.txt"
  let backpacks = lines input
  print $ sum $ priority . uncurry duplicated . splitHalf <$> backpacks
  print $ sum $ priority . duplicated' <$> chunk3 backpacks
