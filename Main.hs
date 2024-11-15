module Main where

import Prelude hiding (lookup)
import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Text.Printf (printf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMaybeMatched)

data Expr
  = Sym String
  | Fun String [Expr]
  deriving Eq

data Rule = Rule
  { hd :: Expr
  , body :: Expr
  }

type Bindings = Map String Expr

instance Show Expr where
  show (Sym s) = s
  show (Fun name args) = printf "%s(%s)" name (intercalate ", " (map show args))

instance Show Rule where
  show Rule { hd, body } = show hd <> " = " <> show body

mergeBindings :: Bindings -> Bindings -> Bindings
mergeBindings = merge preserveMissing preserveMissing (zipWithMaybeMatched combine)
  where
    combine _ v1 v2 | v1 == v2  = Just v1
                    | otherwise = Nothing

substBindings :: Bindings -> Expr -> Expr
substBindings bindings expr =
  case expr of
    Sym name -> fromMaybe expr (Map.lookup name bindings)
    Fun name args ->
      let new_name = 
            case Map.lookup name bindings of
              Nothing -> name
              Just (Sym new_name) -> new_name
              _ -> undefined 
      in
      let new_args = map (substBindings bindings) args in
      Fun new_name new_args

match :: Expr -> Expr -> Bindings
match pattern value =
  case (pattern, value) of
    (Sym s1, _) -> Map.singleton s1 value
    (Fun name1 args1, Fun name2 args2) ->
      if name1 /= name2 || length args1 /= length args2 
         then Map.empty
         else foldr1 mergeBindings $ zipWith match args1 args2
    _ -> Map.empty 

applyAll :: Rule -> Expr -> Expr
applyAll rule expr =
  let bindings = match (hd rule) expr in
      if Map.null bindings then
        case expr of
          Sym _ -> expr
          Fun name args -> Fun name (map (applyAll rule) args)
      else
        substBindings bindings (body rule)

printBindings :: Bindings -> IO ()
printBindings b =
   mapM_ (putStrLn . (\(k, v) -> show k <> " => " <> show v)) (Map.toList b)

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
