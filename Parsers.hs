module Parsers
  ( Parser(..)
  , charP
  , stringP
  , notNull
  , spanP
  , ws
  , sepBy
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
        (f, input') <- p1 input
        (a, input'') <- p2 input'
        Just (f a, input'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP c = Parser $ \inp ->
  case inp of
    x:xs | x == c -> Just (x, xs)
    _             -> Nothing

stringP :: String -> Parser String
stringP = traverse charP

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (x, input') <- p input
  if null x then Nothing
            else Just (x, input')

spanP :: (Char -> Bool) -> Parser String 
spanP pred = Parser $ Just . span pred

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []
