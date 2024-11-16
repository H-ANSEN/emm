module Parsers
  ( Parser(..)
  , charP
  , alphaP
  , stringP
  , notNull
  , spanP
  , ws
  , sepBy
  , anyP
  )
  where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser (p >=> (\(x, xs) -> Just (f x, xs)))

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)
  Parser p1 <*> Parser p2 = Parser f
    where 
      f input = do
        (f', input') <- p1 input
        (a, input'') <- p2 input'
        Just (f' a, input'')

instance Monad Parser where
  (Parser p) >>= f = Parser $ \inp -> do
    (x, inp') <- p inp
    runParser (f x) inp'

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

anyP :: Parser Char
anyP = Parser $ \inp ->
  case inp of
    x:xs -> Just (x, xs)
    []   -> Nothing

charPred :: (Char -> Bool) -> Parser Char
charPred p = anyP >>= (\c -> if p c then pure c else empty)

charP :: Char -> Parser Char
charP c = charPred (c ==)

alphaP :: Parser Char
alphaP = charPred isAlpha

stringP :: String -> Parser String
stringP = traverse charP

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (x, input') <- p input
  if null x then Nothing
            else Just (x, input')

spanP :: (Char -> Bool) -> Parser String 
spanP p = Parser $ Just . span p

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []
