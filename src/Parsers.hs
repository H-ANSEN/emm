module Parsers
  ( Parser
  , StrParser
  , parse
  , maybeParse
  , satisfy
  , sepBy
  , ws
  , charP
  , alphaP
  , digitP
  , stringP
  , spanP
  )
  where

import Data.Char
import Data.List
import Control.Applicative

type Offset = Integer

data ErrorType i e
  = EOF
  | Empty
  | Custom e i
  | Expected i i
  | Unexpected i
  deriving (Show, Eq)

data Error i e = Error
  { erOffset :: Offset
  , erType :: ErrorType i e
  } deriving (Show, Eq)

-- [i] the input stream
-- [e] the type of custom error messages
-- [a] the result of a parsing function
newtype Parser i e a = Parser
  { runParser :: [i] -> Offset -> Either [Error i e] (Offset, a, [i])
  }

type StrParser a = Parser Char String a

instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input offset ->
    case p input offset of
      Left err               -> Left err
      Right (offset', x, xs) -> Right (offset', f x, xs)

instance Applicative (Parser i e) where
  pure val = Parser $ \input offset -> Right (offset, val, input)
  liftA2 f (Parser p1) (Parser p2) = Parser $ \input offset -> do
    (offset', val1, input')   <- p1 input offset
    (offset'', val2, input'') <- p2 input' offset'
    Right (offset'', f val1 val2, input'')

instance Monad (Parser i e) where
  Parser p >>= f = Parser $ \input offset -> do
    (offset', val, input') <- p input offset
    runParser (f val) input' offset'

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ offset -> Left [Error offset Empty]
  (Parser p1) <|> (Parser p2) = Parser $ \input offset ->
    case p1 input offset of
      success@(Right _) -> success
      Left err ->
        case p2 input offset of
          success@(Right _) -> success
          Left err' -> Left $ nub $ err <> err'

parse :: Parser i e b -> [i] -> Either [Error i e] b
parse p input = 
  case runParser p input 0 of
    Left err -> Left err
    Right (_, val, _) -> Right val

maybeParse :: Parser i e a -> [i] -> Maybe a
maybeParse p input =
  case runParser p input 0 of
    Left _ -> Nothing
    Right (_, val, _) -> Just val

token :: (i -> ErrorType i e) -> (i -> Bool) -> Parser i e i
token mkErr predicate = Parser $ \input offset ->
  case input of
    []   -> Left [Error offset EOF]
    x:xs | predicate x -> Right (offset + 1, x, xs)
         | otherwise   -> Left [Error offset $ mkErr x]

satisfy :: (i -> Bool) -> Parser i e i
satisfy = token Unexpected

charP :: Eq i => i -> Parser i e i
charP c = token (Expected c) (== c)

alphaP :: Parser Char String Char
alphaP = token (Custom "Expected alpha character") isAlpha

digitP :: Parser Char String Char
digitP = token (Custom "Expected digit character") isDigit

stringP :: Eq i => [i] -> Parser i e [i]
stringP = traverse charP

spanP :: (Eq i, Eq e) => (i -> Bool) -> Parser i e [i]
spanP predicate = many (token Unexpected predicate)

ws :: (Eq e) => Parser Char e String
ws = spanP isSpace

sepBy :: Alternative f => f a1 -> f a2 -> f [a2]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []
