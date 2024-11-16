module Main where

import Expr

a :: Maybe Expr
a = do
  testRule <- readRule "swap(pair(a, b)) = pair(b, a)"
  testExpr <- readExpr "swap(pair(g(a), f(b)))"
  pure $ applyAll testRule testExpr

main :: IO ()
main = do
  let testRule = readRule "swap(pair(a, b)) = pair(b, a)"
  let testExpr = readExpr "swap(pair(g(a), f(b)))"

  putStrLn $ "Rule:        " <> show testRule
  putStrLn $ "Expr:        " <> show testExpr
  putStrLn $ "Application: " <> show a

