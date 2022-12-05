#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package split
-}

import Data.List.Split

data Move = Move Int Int Int

parseStacksLine :: Int -> String -> [[Char]]
parseStacksLine n is = fmap getOne [0..n-1]
  where
    getChar m = is!!(4*m + 1)
    getOne m =
      let c = getChar m
      in if c == ' ' then [] else [c]

parseStacks :: String -> [[Char]]
parseStacks s = foldl updateStack stacksInit stackLines
  where
    stackLines = drop 1 $ reverse $ lines s
    nStacks = (length (head stackLines) + 1) `div` 4
    stacksInit = replicate nStacks []
    updateStack acc s = zipWith (++) (parseStacksLine nStacks s) acc

parseMove :: String -> Move
parseMove s = Move (read (parts!!1)) (read (parts!!3) - 1) (read (parts!!5) - 1)
  where
    parts = splitOn " " s

parse :: String -> ([[Char]], [Move])
parse s =
  let [stack, moves] = splitOn "\n\n" s
  in (parseStacks stack, parseMove <$> lines moves)

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth n f (h:t)
  | n == 0 = f h:t
  | otherwise = h:modifyNth (n-1) f t

update :: [[Char]] -> [Move] -> [[Char]]
update stacks [] = stacks
update stacks (Move 0 _ _:t) = update stacks t
update stacks (Move n from to:t) = update newStack (Move (n-1) from to:t)
  where
    v = head $ stacks!!from
    newStack = modifyNth to (v:) $ modifyNth from (drop 1) stacks

update' :: [[Char]] -> [Move] -> [[Char]]
update' stacks [] = stacks
update' stacks (Move n from to:t) = update' newStacks t
  where
    boxes = take n (stacks!!from)
    newStacks = modifyNth to (boxes++) $ modifyNth from (drop n) stacks

main :: IO ()
main = do
  input <- readFile "data/05.txt"
  let (stack, moves) = parse input
  print $ head <$> update stack moves
  print $ head <$> update' stack moves
