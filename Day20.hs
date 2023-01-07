#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
-}

rotateToElem :: Eq a => a -> [(a, Int)] -> [(a, Int)]
rotateToElem v ls =
  let (b, a) = span ((/=) v . fst) ls
  in a ++ b

rotateToInd :: Int -> [(a, Int)] -> [(a, Int)]
rotateToInd i ls =
  let (b, a) = span ((/=) i . snd) ls
  in a ++ b

update1 :: [(Int, Int)] -> [(Int, Int)]
update1 [] = []
update1 (h@(v, _):t) =
  let (a, b) = splitAt (v `mod` length t) t
  in a ++ (h:b)

mix :: [(Int, Int)] -> [(Int, Int)]
mix toMix = foldl (\l' i -> update1 $ rotateToInd i l') toMix [0..length toMix - 1]

getCircular :: [a] -> Int -> a
getCircular l p = l!!(p `mod` length l)

main :: IO ()
main = do
  input <- readFile "data/20.txt"
  let list = flip zip [0..] . fmap (\v -> read v :: Int) . lines $ input
  let mixed = rotateToElem 0 $ mix list
  print . sum . fmap (fst . getCircular mixed) $ [1000, 2000, 3000]
  let updated = (\(v, i) -> (v*811589153,i)) <$> list
  let mixed' = rotateToElem 0 . (!! 10) . iterate mix $ updated
  print . sum . fmap (fst . getCircular mixed') $ [1000, 2000, 3000]
