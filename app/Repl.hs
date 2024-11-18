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

type ActionResult = Either ReplContext String

startRepl :: IO ()
startRepl = repl Context { cRules=Map.empty, cExpr=Nothing }

repl :: ReplContext -> IO ()
repl ctx = do
  hFlush stdout
  putStr "emm> "; hFlush stdout
  userInput <- getLine

  let parsedAction = maybeParse parseReplAction userInput
  case processAction ctx parsedAction of
    Right msg  -> do putStrLn $ "ERROR: " <> msg; hFlush stdout; repl ctx
    Left ctx'  ->
      case cExpr ctx' of
        Nothing   -> repl ctx'
        Just expr -> do 
          putStrLn $ "=> " <> show expr; hFlush stdout
          repl ctx'

  pure ()

processAction :: ReplContext -> Maybe ReplAction -> ActionResult
processAction ctx parseResult =
  case parseResult of
    Nothing                 -> Left ctx
    Just Done               -> Left ctx { cExpr=Nothing }
    Just (Shape expr)       -> Left ctx { cExpr=Just expr }
    Just (Def (name, rule)) -> Left ctx { cRules=Map.insert name rule (cRules ctx) }
    Just (Apply name)       -> 
      case cExpr ctx of
        Nothing   -> Right "Attempted to apply rule outside of shaping"
        Just expr ->
          case Map.lookup name (cRules ctx) of
            Nothing   -> Right ("Attempted to apply unbound rule: " <> name)
            Just rule -> Left ctx { cExpr=Just (Expr.applyAll rule expr) }

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
