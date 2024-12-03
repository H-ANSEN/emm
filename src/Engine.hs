module Engine
  ( Expr(..)
  , Rule(..)
  , Context
  , CtxAction
  , CtxResult
  , empty
  , ctxExpr
  , ctxRules
  , processAction
  , parseCtxAction
  , parseExpr
  , parseRule
  , applyAll
  ) 
  where

import Engine.Expr
import Engine.Context

