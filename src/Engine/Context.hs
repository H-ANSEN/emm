-- An extension ontop of the core pattern matching engine found in [Expr.hs]
-- providing a context holding sets of rules and actions on expressions using
-- those rules
module Engine.Context 
  ( Context
  , CtxAction
  , CtxResult
  , Engine.Context.empty
  , ctxExpr
  , processAction
  , parseCtxAction
  )
  where

import Parsers
import Engine.Expr
import Control.Applicative
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)

import qualified Data.Map.Strict as Map

data CtxAction
  = Meta  String MetaRule
  | Def   String Rule (Maybe [String])
  | Shape Expr
  | Apply String
  | Done

data Context = Context 
  { cMetas :: Map String MetaRule,
    cRules :: Map String Rule,
    cExpr  :: Maybe Expr }

type MetaRule = (Rule, Rule)
type CtxResult = Either String Context

empty :: Context
empty = Context {cMetas = Map.empty, cRules = Map.empty, cExpr = Nothing}

ctxExpr :: Context -> Maybe Expr
ctxExpr = cExpr

processAction :: CtxAction -> Context -> CtxResult
processAction action ctx =
  case action of
    Done                 -> Right ctx {cExpr = Nothing}
    Shape expr           -> Right ctx {cExpr = Just expr}
    Meta name (lhs, rhs) -> Right ctx {cMetas = Map.insert name (lhs, rhs) (cMetas ctx)}
    Apply name           ->
      case cExpr ctx of
        Nothing   -> Left "Rule application is not allowed outside of shaping"
        Just expr ->
          case Map.lookup name (cRules ctx) of
            Nothing   -> Left $ name <> " is not a currently bound rule"
            Just rule -> Right ctx {cExpr = Just $ applyAll rule expr}
    Def name rule mMetas ->
      case mMetas of
        Nothing    -> Right ctx {cRules = Map.insert name rule (cRules ctx)}
        Just metas ->
          let metaCtx = cMetas ctx in -- note we disregaurd unknown meta rule names
          let ruleCtx = cRules ctx in -- here by using [mapMaybe] below
          let newRuleCtx = foldr 
                (\(n, r) mmap -> Map.insert (name <> "_" <> n) (createMetaRule r rule) mmap) ruleCtx 
                (mapMaybe (\n -> (,) n <$> Map.lookup n metaCtx) metas) in
              Right ctx {cRules=Map.insert name rule newRuleCtx}

    
createMetaRule :: MetaRule -> Rule -> Rule 
createMetaRule (lhs, rhs) rule =
  let headBindings = match (hd lhs) (hd rule)
      bodyBindings = match (body lhs) (body rule)
      bindings     = mergeBindings headBindings bodyBindings
      newLhs       = substBindings bindings (hd rhs)
      newRhs       = substBindings bindings (body rhs) in
      Rule {hd = newLhs, body = newRhs}
 
{--CONTEXT ACTION PARSERS------------------------------------------------------}

parseCtxAction :: StrParser CtxAction
parseCtxAction =  ruleParser
              <|> metaParser
              <|> shapeParser
              <|> applyParser
              <|> doneParser

identParser :: StrParser String
identParser = do
  first <- alphaP
  rest  <- many (charP '_' <|> alphaP <|> digitP)
  pure (first:rest)

metaParser :: StrParser CtxAction
metaParser = do
  _    <- stringP "meta"                      <* ws
  name <- identParser                         <* ws
  lhs  <- charP '(' *> parseRule <* charP ')' <* ws
  _    <- ws *> charP '='                     <* ws
  rhs  <- charP '(' *> parseRule <* charP ')' <* ws
  pure $ Meta name (lhs, rhs)

ruleParser :: StrParser CtxAction
ruleParser = do
  _    <- stringP "rule" <* ws
  name <- identParser    <* ws
  rule <- parseRule      <* ws
  meta <- optional $ charP '[' *> ws *> sepBy (ws *> charP ',' <* ws) identParser
  pure $ Def name rule meta

shapeParser :: StrParser CtxAction
shapeParser = do
  _ <- stringP "shape" <* ws
  Shape <$> parseExpr

applyParser :: StrParser CtxAction
applyParser = do
  _ <- stringP "apply" <* ws
  Apply <$> identParser

doneParser :: StrParser CtxAction
doneParser = do
  _ <- stringP "done" <* ws
  pure Done
