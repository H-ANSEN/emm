module Repl ( startRepl ) where

import Parsers
import System.IO
import Control.Applicative
import Data.Map.Strict (Map)

import qualified Expr
import qualified Data.Map.Strict as Map

data ReplAction
  = Meta String Expr.Rule Expr.Rule -- Define a meta rule [name (A = B) = (C = D)]
  | Def String Expr.Rule String     -- Define a new rule (name, rule, metas)
  | Shape Expr.Expr                 -- Begin shaping an expression
  | Apply String
  | Done
  deriving Show

data ReplContext = Context {
  cMetas :: Map String (Expr.Rule, Expr.Rule), -- meta rules defined in the ctx
  cRules :: Map String Expr.Rule,              -- rules defined in the context
  cExpr  :: Maybe Expr.Expr                    -- current expression being shaped
} deriving Show

type ActionResult = Either String ReplContext

startRepl :: IO ()
startRepl = repl Context { cMetas = Map.empty,
                           cRules = Map.empty,
                           cExpr  = Nothing }

repl :: ReplContext -> IO ()
repl ctx = do
  hFlush stdout
  putStr "emm> "; hFlush stdout
  userInput <- getLine

  case readInputAction ctx userInput of
    Left errMsg -> do putStrLn errMsg; hFlush stdout; repl ctx
    Right ctx'  ->
      case cExpr ctx' of
        Nothing   -> repl ctx' -- Not currently shaping an expression
        Just expr -> do        -- Currently shaping expression [expr]
          putStrLn $ "=> " <> show expr; hFlush stdout
          repl ctx'

  pure ()

readInputAction :: ReplContext -> String -> ActionResult
readInputAction ctx input =
  case parse parseReplAction input of
    Left err     -> Left (fmtError err)
    Right action -> processAction ctx action

{- EXAMPLE META RULE APPLICATION TO CREATE A NEW RULE
let testRule = Expr.Rule {
  Expr.hd =
    Expr.Fun (Expr.Sym "swap") [Expr.Fun (Expr.Sym "pair") [Expr.Var "A", Expr.Var "B"]],
  Expr.body =
    Expr.Fun (Expr.Sym "pair") [Expr.Var "B", Expr.Var "A"]} in
trace ("swap rule: " <> show testRule) $


let headBindings = Expr.match (Expr.hd rHead) (Expr.hd testRule) in
let bodyBindings = Expr.match (Expr.body rHead) (Expr.body testRule) in
let bindings     = Expr.mergeBindings headBindings bodyBindings in
let new_head     = Expr.substBindings bindings (Expr.hd rBody) in
let new_body     = Expr.substBindings bindings (Expr.body rBody) in
let new_rule     = Expr.Rule {Expr.hd=new_head, Expr.body=new_body} in

trace ("new rule: " <> show new_rule) Right ctx
-}

-- TODO: Parser error while shaping expression causes shaping mode to end
-- TODO: Allow rule to inherit multiple meta rules
-- TODO: Allow meta rules to be completely optional on a rule definition
-- TODO: ??? Maybe move meta rule logic to core engine sense it requires
--       matching, merging and substitution
processAction :: ReplContext -> ReplAction -> ActionResult
processAction ctx parseResult =
  case parseResult of
    Done                  -> Right ctx { cExpr=Nothing }
    Shape expr            -> Right ctx { cExpr=Just expr }
    Def name rule meta    ->
      case Map.lookup meta (cMetas ctx) of
        Nothing             -> Left $ "Using unknown meta rule: " <> meta
        Just (rHead, rBody) ->
          let headBindings  = Expr.match (Expr.hd rHead) (Expr.hd rule) in
          let bodyBindings  = Expr.match (Expr.body rHead) (Expr.body rule) in
          let bindings      = Expr.mergeBindings headBindings bodyBindings in
          let new_head      = Expr.substBindings bindings (Expr.hd rBody) in
          let new_body      = Expr.substBindings bindings (Expr.body rBody) in
          let new_rule      = Expr.Rule {Expr.hd=new_head, Expr.body=new_body} in
          let new_rule_name = name <> "_" <> meta in
          Right ctx { cRules = Map.insert name rule $
                               Map.insert new_rule_name new_rule (cRules ctx) }

    Meta name rHead rBody -> Right ctx { cMetas=Map.insert name (rHead, rBody) (cMetas ctx) }
    Apply name            ->
      case cExpr ctx of
        Nothing   -> Left "Attempted to apply rule outside of shaping"
        Just expr ->
          case Map.lookup name (cRules ctx) of
            Nothing   -> Left ("Attempted to apply unbound rule: " <> name)
            Just rule -> Right ctx { cExpr=Just (Expr.applyAll rule expr) }

{--REPL COMMAND PARSERS--------------------------------------------------------}

parseReplAction :: StrParser ReplAction
parseReplAction = ruleParser  <|> 
                  metaParser  <|>
                  shapeParser <|> 
                  applyParser <|> 
                  doneParser

identParser :: StrParser String
identParser = do
  first <- alphaP
  rest  <- many (charP '_' <|> alphaP <|> digitP)
  pure (first:rest)

metaParser :: StrParser ReplAction
metaParser = do
  _      <- stringP "meta"                           <* ws
  name   <- identParser                              <* ws
  ruleHd <- charP '(' *> Expr.parseRule <* charP ')' <* ws
  _      <- ws *> charP '='                          <* ws
  ruleTl <- charP '(' *> Expr.parseRule <* charP ')' <* ws
  pure $ Meta name ruleHd ruleTl

ruleParser :: StrParser ReplAction
ruleParser = do
  _    <- stringP "rule" <* ws
  name <- identParser    <* ws
  rule <- Expr.parseRule <* ws
  meta <- charP '[' *> ws *> identParser <* ws <* charP ']'
  pure $ Def name rule meta

shapeParser :: StrParser ReplAction
shapeParser = do
  _ <- stringP "shape" <* ws
  Shape <$> Expr.parseExpr

applyParser :: StrParser ReplAction
applyParser = do
  _ <- stringP "apply" <* ws
  Apply <$> identParser

doneParser :: StrParser ReplAction
doneParser = do
  _ <- stringP "done" <* ws
  pure Done
