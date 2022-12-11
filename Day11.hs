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
  operation :: Int -> Int,
  test :: Int,
  sendHappy :: Int,
  sendSad :: Int
}

parseMonkey :: String -> ([Int], Monkey)
parseMonkey s =
  let ls = lines s
      elems = fmap read . splitOn ", " . drop 1 . dropWhile (/=':') $ ls!!1
      lastInt = read . last . splitOn " "
      op ["old", "*"] = (^ 2)
      op [n, "*"] = (* read n)
      op l = (+) . read . head $ l
  in (elems, Monkey { operation = op . take 2 . reverse . splitOn " " $ (ls!!2) , test = lastInt (ls!!3), sendHappy = lastInt (ls!!4), sendSad = lastInt (ls!!5)})

processMonkey :: Monkey -> [Int] -> ([Int], [Int])
processMonkey m = partition (\v -> v `rem` test m == 0) . fmap ((`div` 3) . operation m)

processMonkey' :: Int -> Monkey -> [Int] -> ([Int], [Int])
processMonkey' d m = partition (\v -> v `rem` test m == 0) . fmap ((`mod` d) . operation m)

processRound :: (Monkey -> [Int] -> ([Int], [Int])) -> [Monkey] -> [(Int, [Int])] -> [(Int, [Int])]
processRound process ms state = foldl update state (zip [0..] ms)
  where
    update :: [(Int, [Int])] -> (Int, Monkey) -> [(Int, [Int])]
    update s (i, m) =
      let (n, c) = s!!i
          (happy, sad) = process m c
      in (s & ix i .~ (n + length c, []) & ix (sendHappy m) . _2 %~ (++happy) & ix (sendSad m) . _2 %~ (++sad))

score :: [Int] -> Int
score = product . take 2 . reverse . sort

main :: IO ()
main = do
  input <- readFile "data/11.txt"
  let input' = fmap parseMonkey . splitOn "\n\n" $ input
  let (initialState, monkeys) = unzip input' & _1 %~ fmap (0,)
  print . score . fmap fst . (!! 20) . iterate (processRound processMonkey monkeys) $ initialState
  let range = foldl1 lcm (fmap test monkeys)
  print . score . fmap fst . (!! 10000) . iterate (processRound (processMonkey' range) monkeys) $ initialState
