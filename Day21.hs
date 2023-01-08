#!/usr/bin/env stack
{-
  stack script
  --resolver lts-20.2
  --package split
  --package containers
-}

import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M

data Expr = Const Int
          | Ref String
          | Expr Char Expr Expr
          | Var deriving Show

parse :: String -> (String, Expr)
parse s
  | length parts == 2 = (name, Const (read (parts!!1)))
  | otherwise = (name, Expr (head $ parts!!2) (Ref (parts!!1)) (Ref (parts!!3)))
  where
    parts = splitOn " " s
    name = takeWhile (/=':') (head parts)

simplify :: M.Map String Expr -> Expr -> Expr
simplify refs (Ref ref) = simplify refs (fromJust $ M.lookup ref refs)
simplify refs (Expr op a b) =
  let sa = simplify refs a
      sb = simplify refs b
      value (Const x) (Const y)
        | op == '*' = Const (x * y)
        | op == '+' = Const (x + y)
        | op == '-' = Const (x - y)
        | op == '/' = Const (x `div` y)
        | otherwise = error "unknown operation"
      value x y = Expr op x y
  in value sa sb
simplify _ expr = expr

solve :: Expr -> Expr -> Int
solve l@(Const _) r = solve r l
solve Var (Const r) = r
solve (Expr op (Const l) l2) (Const r)
  | op == '*' = solve l2 (Const (r `div` l))
  | op == '+' = solve l2 (Const (r - l))
  | op == '-' = solve l2 (Const (-1 * (r - l)))
  | op == '/' = error "need rationals"
  | otherwise = error "unknown operation"
solve (Expr op l1 (Const l)) (Const r)
  | op == '*' = solve l1 (Const (r `div` l))
  | op == '+' = solve l1 (Const (r - l))
  | op == '-' = solve l1 (Const (r + l))
  | op == '/' = solve l1 (Const (r * l))
  | otherwise = error "unknown operation"
solve _ _ = error "this is too hard, solve by hand :)"

main :: IO ()
main = do
  input <- readFile "data/21.txt"
  let monkeys = M.fromList . fmap parse . lines $ input
  let Const solution = simplify monkeys (Ref "root")
  print solution
  let Expr _ l r = fromJust $ M.lookup "root" monkeys
  let monkeysWithInput = M.insert "humn" Var monkeys
  print $ solve (simplify monkeysWithInput l) (simplify monkeysWithInput r)
