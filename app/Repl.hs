module Repl ( startRepl ) where

import Parsers
import System.IO
import qualified Engine

startRepl :: IO ()
startRepl = repl Engine.empty

repl :: Engine.Context -> IO ()
repl ctx = do
  hFlush stdout
  putStr "emm> "; hFlush stdout
  userInput <- getLine

  if userInput == ""
     then repl ctx
     else

    case readInputAction ctx userInput of
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
   
-- TODO: Parser error while shaping expression causes shaping mode to end
