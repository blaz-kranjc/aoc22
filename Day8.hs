#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package containers
  --package vector
-}

import Data.Char
import qualified Data.Set as Set
import Data.Vector ((!), Vector)
import qualified Data.Vector as Vector

data Matrix = Matrix {
  entries :: Vector Int,
  nRows :: Int,
  nColumns :: Int
}

parse :: String -> Matrix
parse s = Matrix { entries = out , nRows = length rows, nColumns = length . head $ rows }
  where
    rows = lines s
    out = Vector.fromList . concatMap (fmap digitToInt) $ rows

row :: Int -> Matrix -> [Int]
row n m = (!) (entries m) . (+) (nColumns m * n) <$> [0..(nColumns m - 1)]

column :: Int -> Matrix -> [Int]
column n m = (!) (entries m) . (\i -> nRows m * i + n) <$> [0..(nRows m - 1)]

visible :: [Int] -> [Int]
visible ls = fst . foldl visible' ([0], head ls) . zip [1..] . drop 1 $ ls
  where
    visible' acc@(visible, height) (index, value)
      | value > height = (index:visible, value)
      | otherwise = acc

sideVisible :: Matrix -> Set.Set (Int, Int)
sideVisible m@Matrix{nRows=nRows, nColumns=nColumns} = Set.fromList $ vl ++ vr ++ vt ++ vb
  where
    vl = concatMap (\r -> fmap (r,) . visible $ row r m) [0..(nRows-1)]
    vr = concatMap (\r -> fmap ((r,) . (nColumns-1-)) . visible . reverse $ row r m) [0..(nRows-1)]
    vt = concatMap (\c -> fmap (,c) . visible $ column c m) [0..(nColumns-1)]
    vb = concatMap (\c -> fmap ((,c) . (nRows-1-)) . visible . reverse $ column c m) [0..(nColumns-1)]

countVisible :: Int -> [Int] -> Int
countVisible tree = length . foldl go []
  where
    go [] v = [v]
    go a@(h:_) v
      | h < tree = v:a
      | otherwise = a

visibleScore :: Matrix -> (Int, Int) -> Int
visibleScore m@Matrix{nRows=nRows, nColumns=nColumns} (r, c) = vl * vr * vt * vb
  where
    vl = countVisible t . drop (c+1) . flip row m $ r
    vr = countVisible t . drop (nColumns-c) . reverse . flip row m $ r
    vt = countVisible t . drop (r+1) . flip column m $ c
    vb = countVisible t . drop (nRows-r) . reverse . flip column m $ c
    t = entries m ! (nColumns * r + c)

main :: IO ()
main = do
  input <- readFile "data/08.txt"
  let matrix = parse input
  print . length . sideVisible $ matrix
  let pairs = [(r, c) | r <- [0..(nRows matrix - 1)], c <- [0..(nColumns matrix - 1)]]
  print . maximum . fmap (visibleScore matrix) $ pairs
