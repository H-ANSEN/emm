module Repl ( startRepl ) where

import Parsers
import System.IO
import System.Exit
import Control.Monad (forM_)

import qualified Engine

startRepl :: IO ()
startRepl = repl Engine.empty

repl :: Engine.Context -> IO ()
repl ctx = do
  hFlush stdout
  putStr "emm> "; hFlush stdout
  input <- getLine

  if input == "" then repl ctx
  else if head input == ':' then readReplAction ctx (tail input) >>= repl 
  else
    case readInputAction ctx input of
      Left errMsg -> do putStrLn errMsg; hFlush stdout; repl ctx
      Right ctx'  ->
        case Engine.ctxExpr ctx' of
          Nothing   -> repl ctx' -- Not currently shaping an expression
          Just expr -> do        -- Currently shaping expression [expr]
            putStrLn $ "=> " <> show expr; hFlush stdout
            repl ctx'

  pure ()

readInputAction :: Engine.Context -> String -> Engine.CtxResult
readInputAction ctx input =
  case parse Engine.parseCtxAction input of
    Left err     -> Left (fmtError err)             -- parse error
    Right action -> Engine.processAction action ctx -- user error or new context

-- process any repl action beginning with ':' character
readReplAction :: Engine.Context -> String -> IO Engine.Context
readReplAction ctx input =
  case input of
    "list" ->
      let rules = Engine.ctxRules ctx in
      let longestNameLen = maximum $ map (length . fst) rules in
      forM_ (Engine.ctxRules ctx) (\(name, rule) -> 
        let padding = replicate (longestNameLen - length name) ' ' in
        putStrLn $ name ++ ": " ++ padding ++ show rule)
      >> pure ctx
    "quit" -> exitSuccess
    _      -> pure ctx 
   
-- TODO: Parser error while shaping expression causes shaping mode to end
