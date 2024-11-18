module Repl ( startRepl ) where

import Parsers
import System.IO
import Control.Applicative
import Data.Map.Strict (Map)

import qualified Expr
import qualified Data.Map.Strict as Map

data ReplAction
  = Def (String, Expr.Rule) -- Define a new rule (name, rule)
  | Shape Expr.Expr         -- Begin shaping an expression
  | Apply String
  | Done
  deriving Show

data ReplContext = Context {
  cRules :: Map String Expr.Rule, -- rules defined in the context
  cExpr  :: Maybe Expr.Expr       -- current expression being shaped
} deriving Show

type ActionResult = Either String ReplContext

startRepl :: IO ()
startRepl = repl Context { cRules=Map.empty, cExpr=Nothing }

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

processAction :: ReplContext -> ReplAction -> ActionResult
processAction ctx parseResult =
  case parseResult of
    Done             -> Right ctx { cExpr=Nothing }
    Shape expr       -> Right ctx { cExpr=Just expr }
    Def (name, rule) -> Right ctx { cRules=Map.insert name rule (cRules ctx) }
    Apply name       ->
      case cExpr ctx of
        Nothing   -> Left "Attempted to apply rule outside of shaping"
        Just expr ->
          case Map.lookup name (cRules ctx) of
            Nothing   -> Left ("Attempted to apply unbound rule: " <> name)
            Just rule -> Right ctx { cExpr=Just (Expr.applyAll rule expr) }

{--REPL COMMAND PARSERS--------------------------------------------------------}

parseReplAction :: StrParser ReplAction
parseReplAction = ruleParser <|> shapeParser <|> applyParser <|> doneParser

identParser :: StrParser String
identParser = do
  first <- alphaP
  rest  <- many (charP '_' <|> alphaP <|> digitP)
  pure (first:rest)

ruleParser :: StrParser ReplAction
ruleParser = do
  _    <- stringP "rule" <* ws
  name <- identParser    <* ws
  rule <- Expr.parseRule <* ws
  pure $ Def (name, rule)

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
