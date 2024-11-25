module Engine.Expr where

import Parsers
import Prelude hiding (lookup, head)
import Text.Printf (printf)
import Control.Applicative
import Data.Functor
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
  | Op OpKind Expr Expr
  deriving (Eq, Show)

data OpKind = Add | Sub | Mul | Div | Pow
  deriving (Eq, Show)

data Rule = Rule
  { hd :: Expr
  , body :: Expr
  }

type Bindings = Map String Expr

{-
instance Show OpKind where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

instance Show Expr where
  show (Sym s) = s
  show (Var s) = s
  show (Fun head args) = printf "%s(%s)" (show head) (intercalate ", " (map show args))
  show (Op kind lhs rhs) = printf "%s %s %s" (show lhs) (show kind) (show rhs)

instance Show Rule where
  show Rule { hd=mHead, body=mBody} = show mHead <> " = " <> show mBody
-}

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
    Sym _           -> expr
    Var name        -> fromMaybe expr (Map.lookup name bindings)
    Fun head args   ->
      let newHead = substBindings bindings head
          newArgs = map (substBindings bindings) args in
          Fun newHead newArgs
    Op kind lhs rhs ->
      let newLhs = substBindings bindings lhs
          newRhs = substBindings bindings rhs in
          Op kind newLhs newRhs
           
-- Pattern match across two expressions creating a set of bindings between the
-- two
match :: Expr -> Expr -> Bindings
match pattern value =
  case (pattern, value) of
    (Var s1, _) -> Map.singleton s1 value
    (Sym s1, Sym s2) -> if s1 == s2 then Map.singleton s1 value else Map.empty
    (Fun name1 args1, Fun name2 args2) ->
      if name1 == name2 || length args1 == length args2 
         then foldr1 mergeBindings $ zipWith match args1 args2
         else Map.empty
    (Op kind1 lhs1 rhs1, Op kind2 lhs2 rhs2) ->
      if kind1 == kind2
         then mergeBindings (match lhs1 lhs2) (match rhs1 rhs2)
         else Map.empty
    _ -> Map.empty 

-- Given a rule and an expression attempt to apply the rule to the expression
applyAll :: Rule -> Expr -> Expr
applyAll rule expr =
  let bindings = match (hd rule) expr in
      if Map.null bindings then
        case expr of
          Sym _ -> expr
          Var _ -> expr
          Fun head args   -> Fun (applyAll rule head) (map (applyAll rule) args)
          Op kind lhs rhs -> Op kind (applyAll rule lhs) (applyAll rule rhs)
      else
         substBindings bindings (body rule)

{--PARSER----------------------------------------------------------------------}

type GenericParser a = Parser Char String a
type OpParser a = GenericParser (a -> a -> a) -- lhs -> rhs -> Op
type ExprParser = GenericParser Expr

parseRule :: StrParser Rule
parseRule = do
  mHead <- parseExpr
  _     <- ws *> charP '=' <* ws
  mBody <- parseExpr
  pure Rule { hd=mHead, body=mBody }

parseLeftAssoc :: GenericParser a -> OpParser a -> GenericParser a
parseLeftAssoc termParser opParser = do
  first <- termParser
  rest  <- many ((,) <$> opParser <*> termParser)
  pure $ foldl (\acc (operator, next) -> operator acc next) first rest

parseExpr :: GenericParser Expr
parseExpr = parseLeftAssoc parseFactor (plus <|> minus) 
  where plus = ws *> charP '+' $> Op Add <* ws
        minus = ws *> charP '-' $> Op Sub <* ws

parseFactor :: GenericParser Expr
parseFactor = parseLeftAssoc parsePow (mulP <|> divP) 
  where mulP = ws *> charP '*' $> Op Mul <* ws
        divP = ws *> charP '/' $> Op Div <* ws

parsePow :: GenericParser Expr
parsePow = parseLeftAssoc parseTerm sym
  where sym = ws *> charP '^' $> Op Pow <* ws

parseTerm :: StrParser Expr
parseTerm = parseFun <|> parseSymVar

parseFun :: StrParser Expr
parseFun = do
  name <- parseSymVar
  Fun name <$> commaSepArgs

parseSymVar :: StrParser Expr
parseSymVar = do
  first <- alphaP <|> digitP
  rest  <- many (charP '_' <|> alphaP <|> digitP)
  if isUpper first then pure (Var $ first:rest)
                   else pure (Sym $ first:rest)

commaSepArgs :: StrParser [Expr]
commaSepArgs = 
  charP '(' *> ws *> sepBy (ws *> charP ',' <* ws) parseTerm <* ws <* charP ')'
