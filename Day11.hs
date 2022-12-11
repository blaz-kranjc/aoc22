#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package microlens
  --package split
-}

import Data.List
import Data.List.Split
import Lens.Micro

data Monkey = Monkey {
  itemUpdate :: Int -> Int,
  itemCheck :: Int,
  sendTrue :: Int,
  sendFalse :: Int
}

parseMonkey :: String -> ([Int], Monkey)
parseMonkey s =
  let ls = lines s
      lastInt = read . last . splitOn " "
      op ["old", "*"] = (^ 2)
      op [n, "*"] = (* read n)
      op l = (+) . read . head $ l
  in (
    fmap read . splitOn ", " . drop 1 . dropWhile (/=':') $ ls!!1,
    Monkey {
      itemUpdate = op . take 2 . reverse . splitOn " " $ (ls!!2),
      itemCheck = lastInt (ls!!3),
      sendTrue = lastInt (ls!!4),
      sendFalse = lastInt (ls!!5)
    }
  )

processMonkey :: (Int -> Int) -> Monkey -> [Int] -> ([Int], [Int])
processMonkey op m = partition (\v -> v `rem` itemCheck m == 0) . fmap (op . itemUpdate m)

processRound :: (Int -> Int) -> [Monkey] -> [(Int, [Int])] -> [(Int, [Int])]
processRound op ms state = foldl update state (zip [0..] ms)
  where
    update :: [(Int, [Int])] -> (Int, Monkey) -> [(Int, [Int])]
    update s (i, m) =
      let (n, c) = s!!i
          (pass, fail) = processMonkey op m c
      in (s & ix i .~ (n + length c, []) & ix (sendTrue m) . _2 %~ (++pass) & ix (sendFalse m) . _2 %~ (++fail))

main :: IO ()
main = do
  input <- readFile "data/11.txt"
  let input' = fmap parseMonkey . splitOn "\n\n" $ input
  let (initialState, monkeys) = unzip input' & _1 %~ fmap (0,)
  let score = product . take 2 . reverse . sort
  print . score . fmap fst . (!! 20) . iterate (processRound (`div` 3) monkeys) $ initialState
  let range = foldl1 lcm (fmap itemCheck monkeys)
  print . score . fmap fst . (!! 10000) . iterate (processRound (`mod` range) monkeys) $ initialState
