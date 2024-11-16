module Expr 
  ( Expr(..)
  , Rule(..)
  , applyAll
  , readRule
  , readExpr
  )
  where

import Parsers
import Prelude hiding (lookup)
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

-- Pattern match across two expressions creating a set of bindings between the
-- two
match :: Expr -> Expr -> Bindings
match pattern value =
  case (pattern, value) of
    (Sym s1, _) -> Map.singleton s1 value
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
          Fun name args -> Fun name (map (applyAll rule) args)
      else
         substBindings bindings (body rule)

printBindings :: Bindings -> IO ()
printBindings b =
   mapM_ (putStrLn . (\(k, v) -> show k <> " => " <> show v)) (Map.toList b)

{--PARSER----------------------------------------------------------------------}

parseIdent :: Parser String
parseIdent = do
  first <- charP '_' <|> alphaP
  rest  <- many (charP '_' <|> alphaP)
  pure (first : rest)

parseSym :: Parser Expr
parseSym = Sym <$> parseIdent

parseFun :: Parser Expr
parseFun = do
  name <- parseIdent
  args <- charP '(' *> sepBy (ws *> charP ',' <* ws)  parseExpr <* charP ')'
  pure (Fun name args)

parseExpr :: Parser Expr
parseExpr = parseFun <|> parseSym

parseRule :: Parser Rule
parseRule = do
  hd   <- parseExpr
  ws *> charP '=' <* ws
  body <- parseExpr
  pure Rule { hd, body }

readRule :: String -> Maybe Rule
readRule str = fst <$> runParser parseRule str

readExpr :: String -> Maybe Expr
readExpr str = fst <$> runParser parseExpr str
