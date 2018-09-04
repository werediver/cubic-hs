module Parser (
    Parser (..),
    runParser
  ) where

import Control.Applicative
import Control.Monad
import Data.Functor ((<&>))

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

runParser :: Parser a -> String -> Maybe a
runParser p s =
  parse p s <&> fst

instance Functor Parser where
  fmap f p = Parser $ \s -> fmap (\(a, s') -> (f a, s')) $ parse p s

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) p q = Parser $ \s ->
    case parse p s of
      Nothing -> parse q s
      smth    -> smth

instance Monad Parser where
  (>>=) p f = Parser (\s -> parse p s >>= \(a, s') -> parse (f a) s')
