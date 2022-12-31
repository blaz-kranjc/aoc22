#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package split
  --package microlens
-}

import Data.List
import Data.List.Split
import Data.Maybe
import Lens.Micro

data PacketPart = Val Int | Arr [PacketPart]

instance Eq PacketPart where
  (Val l) == (Val r) = l == r
  (Arr ls) == (Arr rs) = ls == rs
  l@(Val _) == (Arr rs) = [l] == rs
  l == r = r == l

instance Ord PacketPart where
  (Val l) `compare` (Val r) = l `compare` r
  (Arr ls) `compare` (Arr rs) = ls `compare` rs
  l@(Val _) `compare` (Arr rs) = [l] `compare` rs
  (Arr ls) `compare` r@(Val _) = ls `compare` [r]

parsePacket :: String -> PacketPart
parsePacket s =
  let (Arr result) = parseAcc [[]] s
  in head result
  where
    updateStackN "" stack = stack
    updateStackN n stack = stack & _head %~ ((Val $ read n):)
    updateStackNext (Just ']') stack = tail stack & _head %~ ((Arr $ reverse . head $ stack):)
    updateStackNext (Just '[') stack = []:stack
    updateStackNext _ stack = stack
    parseAcc stack "" = Arr $ reverse . head $ stack
    parseAcc stack s =
      let (n, rest) = span (`notElem` ",[]") s
          next = listToMaybe rest
      in parseAcc (updateStackNext next . updateStackN n $ stack) (drop 1 rest)

sortIndexes :: [PacketPart] -> (Int, Int)
sortIndexes ps =
  let s1 = Arr [Arr [Val 2]]
      s2 = Arr [Arr [Val 6]]
      ss = sort $ s1:s2:ps
  in ((+) 1 $ fromJust $ s1 `elemIndex` ss, (+) 1 $ fromJust $ s2 `elemIndex` ss)


main :: IO ()
main = do
  input <- readFile "data/13.txt"
  let packets = fmap (fmap parsePacket . lines) . splitOn "\n\n" $ input
  print . sum . fmap fst . filter ((\v -> v!!0 < v!!1) . snd) . zip [1..] $ packets
  print . uncurry (*) . sortIndexes . concat $ packets
