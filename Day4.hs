#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
-}

tokenize :: String -> [String] -> String -> [String]
tokenize acc accs [] = reverse $ (reverse acc):accs
tokenize acc accs (',':t) = tokenize [] ((reverse acc):accs) t
tokenize acc accs ('-':t) = tokenize [] ((reverse acc):accs) t
tokenize acc accs (h:t) = tokenize (h:acc) accs t

parse :: String -> ((Int, Int), (Int, Int))
parse s =
  let [a, b, c, d] = tokenize [] [] s
  in ((read a, read b), (read c, read d))

fullyOverlaps :: ((Int, Int), (Int, Int)) -> Bool
fullyOverlaps ((a, b), (c, d)) = ((a <= c) && (b >= d)) || ((c <= a) && (d >= b))

overlaps :: ((Int, Int), (Int, Int)) -> Bool
overlaps ((a, b), (c, d)) = ((c >= a) && (c <= b)) || ((a >= c) && (a <= d))

main :: IO ()
main = do
  input <- readFile "data/04.txt"
  let ranges = parse <$> lines input
  print $ length $ filter fullyOverlaps ranges
  print $ length $ filter overlaps ranges
