module Main where

import Expr

swap = Rule { 
  hd = Fun "swap" [Fun "pair" [Sym "a", Sym "b"]], 
  body = Fun "pair" [Sym "b", Sym "a"] 
}

expr = Fun "swap" [Fun "pair" [Fun "f" [Sym "a"], Fun "g" [Sym "b"]]]

main :: IO ()
main = do
  putStrLn $ "Rule:    " <> show swap
  putStrLn $ "Expr:    " <> show expr
  putStrLn $ "Applied: " <> show (applyAll swap expr)
