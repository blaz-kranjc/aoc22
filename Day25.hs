#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
-}

import Data.Char

fromSnafuDigit :: Char -> Integer
fromSnafuDigit c
  | c == '-' = -1
  | c == '=' = -2
  | otherwise = toInteger . digitToInt $ c

fromSnafu :: String -> Integer
fromSnafu = foldl1 (\a v -> a * 5 + v) . fmap fromSnafuDigit

toSnafuDigit :: Integer -> Char
toSnafuDigit i
  | i == -1 = '-'
  | i == -2 = '='
  | otherwise = chr (ord '0' + fromIntegral i)

toSnafu :: Integer -> String
toSnafu 0 = ""
toSnafu i
  | rem <= 2 = toSnafu (i `div` 5) ++ [toSnafuDigit rem]
  | otherwise = toSnafu ((i + (5 - rem)) `div` 5) ++ [toSnafuDigit (rem - 5)]
  where rem = i `mod` 5

main :: IO ()
main = do
  input <- readFile "data/25.txt"
  print . toSnafu . sum . fmap fromSnafu . lines $ input
