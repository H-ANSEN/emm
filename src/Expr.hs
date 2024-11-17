module Expr 
  ( Expr(..)
  , Rule(..)
  , applyAll
  , parseRule
  , parseExpr
  )
  where

import Parsers
import Prelude hiding (lookup, head)
import Text.Printf (printf)
import Control.Applicative
import Data.Char
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMaybeMatched)

import qualified Data.Map.Strict as Map

data Expr
  = Sym String
  | Var String
  | Fun Expr [Expr]
  deriving Eq

data Rule = Rule
  { hd :: Expr
  , body :: Expr
  }

type Bindings = Map String Expr

instance Show Expr where
  show (Sym s) = s
  show (Var s) = s
  show (Fun head args) = printf "%s(%s)" (show head) (intercalate ", " (map show args))

instance Show Rule where
  show Rule { hd=mHead, body=mBody} = show mHead <> " = " <> show mBody

-- Merge two bindings together removing duplicate keys binding different values
mergeBindings :: Bindings -> Bindings -> Bindings
mergeBindings = merge preserveMissing 
                      preserveMissing 
                      (zipWithMaybeMatched combine)
  where
    combine _ v1 v2 | v1 == v2  = Just v1
                    | otherwise = Nothing

-- Substitue bound values in the given expression using the given bindings
substBindings :: Bindings -> Expr -> Expr
substBindings bindings expr =
  case expr of
    Sym _ -> expr
    Var name -> fromMaybe expr (Map.lookup name bindings)
    Fun head args ->
      let new_head = substBindings bindings head
          new_args = map (substBindings bindings) args in
      Fun new_head new_args

-- Pattern match across two expressions creating a set of bindings between the
-- two
match :: Expr -> Expr -> Bindings
match pattern value =
  case (pattern, value) of
    (Var s1, _) -> Map.singleton s1 value
    (Sym s1, Sym s2) -> if s1 == s2 then Map.singleton s1 value else Map.empty
    (Fun name1 args1, Fun name2 args2) ->
      if name1 /= name2 || length args1 /= length args2 
         then Map.empty
         else foldr1 mergeBindings $ zipWith match args1 args2
    _ -> Map.empty 

-- Given a rule and an expression attempt to apply the rule to the expression
applyAll :: Rule -> Expr -> Expr
applyAll rule expr =
  let bindings = match (hd rule) expr in
      if Map.null bindings then
        case expr of
          Sym _ -> expr
          Var _ -> expr
          Fun head args -> Fun (applyAll rule head) (map (applyAll rule) args)
      else
         substBindings bindings (body rule)

{--PARSER----------------------------------------------------------------------}

parseSymVar :: Parser Expr
parseSymVar = do
  first <- alphaP <|> digitP -- [isUpper] will distinguish this case
  rest  <- many (charP '_' <|> alphaP <|> digitP)
  if isUpper first then parseFun (Var $ first:rest) <|> pure (Var $ first:rest)
                   else parseFun (Sym $ first:rest) <|> pure (Sym $ first:rest)

parseFun :: Expr -> Parser Expr
parseFun name = do
  args <- charP '(' *> ws *>
          sepBy (ws *> charP ',' <* ws)
          parseSymVar <* ws <* charP ')'
  pure $ Fun name args

parseExpr :: Parser Expr
parseExpr = parseSymVar

parseRule :: Parser Rule
parseRule = do
  mHead <- parseExpr
  _     <- ws *> charP '=' <* ws
  mBody <- parseExpr
  pure Rule { hd=mHead, body=mBody }
