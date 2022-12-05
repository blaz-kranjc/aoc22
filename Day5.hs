#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package split
  --package microlens
-}

import Data.List.Split
import Lens.Micro

data Move = Move Int Int Int

parseStacksLine :: Int -> String -> [[Char]]
parseStacksLine n is = (\m -> filter (/= ' ') [is!!(4*m+1)]) <$> [0..n-1]

parseStacks :: String -> [[Char]]
parseStacks s = foldl updateStack (replicate nStacks []) stackLines
  where
    stackLines = drop 1 $ reverse $ lines s
    nStacks = (length (head stackLines) + 1) `div` 4
    updateStack acc s = zipWith (++) (parseStacksLine nStacks s) acc

parseMove :: String -> Move
parseMove s =
  let parts = splitOn " " s
  in Move (read (parts!!1)) (read (parts!!3) - 1) (read (parts!!5) - 1)

parse :: String -> ([[Char]], [Move])
parse s =
  let [stack, moves] = splitOn "\n\n" s
  in (parseStacks stack, parseMove <$> lines moves)

updateByOne :: [[Char]] -> Move -> [[Char]]
updateByOne stacks (Move n from to) = iterate update' stacks !! n
  where
    update' s = s & ix to %~ ((head $ s!!from):) & ix from %~ drop 1

updateGroup :: [[Char]] -> Move -> [[Char]]
updateGroup stacks (Move n from to) =
  let group = take n (stacks!!from)
  in stacks & ix to %~ (group++) & ix from %~ drop n

main :: IO ()
main = do
  input <- readFile "data/05.txt"
  let (stack, moves) = parse input
  print $ head <$> foldl updateByOne stack moves
  print $ head <$> foldl updateGroup stack moves
