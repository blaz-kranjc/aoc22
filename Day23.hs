#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package containers
-}

import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

parse :: String -> [(Int, Int)]
parse =
  let toPoints i = fmap (\(j, _) -> (j, i)) . filter (\v -> snd v == '#')
  in concatMap (uncurry toPoints) . zip [0..] . fmap (zip [0..]) . lines

boundingBox :: [(Int, Int)] -> ((Int, Int), (Int, Int))
boundingBox [] = ((0, 0), (0, 0))
boundingBox ((x0, y0):t) =
  let update ((xm, xM), (ym, yM)) (x, y) = ((min xm x, max xM x), (min ym y, max yM y))
  in foldl update ((x0, x0), (y0, y0)) t

empties :: [(Int, Int)] -> Int
empties l =
  let ((xm, xM), (ym, yM)) = boundingBox l
  in (xM - xm + 1) * (yM - ym + 1) - length l

rotate :: Int -> [a] -> [a]
rotate i l =
  let (a, b) = splitAt (i `mod` length l) l
  in b ++ a

simulate :: Int -> S.Set (Int, Int) -> S.Set (Int, Int)
simulate i s =
  let planned = foldl plan M.empty (S.toList s)
      freeN (x, y) s = S.notMember (x, y-1) s && S.notMember (x-1, y-1) s && S.notMember (x+1, y-1) s
      freeS (x, y) s = S.notMember (x, y+1) s && S.notMember (x-1, y+1) s && S.notMember (x+1, y+1) s
      freeW (x, y) s = S.notMember (x-1, y) s && S.notMember (x-1, y-1) s && S.notMember (x-1, y+1) s
      freeE (x, y) s = S.notMember (x+1, y) s && S.notMember (x+1, y-1) s && S.notMember (x+1, y+1) s
      originalOrder = [ (freeN, \(x, y) -> (x, y-1)) , (freeS, \(x, y) -> (x, y+1))
              , (freeW, \(x, y) -> (x-1, y)) , (freeE, \(x, y) -> (x+1, y)) ]
      order = rotate i originalOrder
      go to from m = M.insert to (from:fromMaybe [] (M.lookup to m)) m
      plan m p
        | isNothing dir || all (\(c, _) -> c p s) order = go p p m
        | otherwise = go ((snd $ fromJust dir) p) p m
        where dir = find (\(check, _) -> check p s) order
      merge oks fails = concatMap snd (M.toList fails) ++ (fst <$> M.toList oks)
  in S.fromList . uncurry merge $ M.partition ((==) 1 . length) planned

firstUnchangedIndex :: Eq a => Int -> [a] -> Int
firstUnchangedIndex _ [] = -1
firstUnchangedIndex _ [x] = -1
firstUnchangedIndex i (x:y:t)
  | x == y = i
  | otherwise = firstUnchangedIndex (i+1) (y:t)

main :: IO ()
main = do
  input <- readFile "data/23.txt"
  let initial = S.fromList . parse $ input
  let ticks = fst <$> iterate (\(s, i) -> (simulate i s, i + 1)) (initial, 0)
  print . empties . S.toList $ ticks!!10
  print $ firstUnchangedIndex 1 ticks
