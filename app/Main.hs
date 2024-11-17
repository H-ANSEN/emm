module Main where

import System.IO
import qualified Expr

main :: IO ()
main = do
  putStr "> " 
  hFlush stdout
  input <- getLine
  case Expr.readExpr input of
    Just expr -> do print expr; main
    Nothing   -> do putStrLn "invaild input"; main
