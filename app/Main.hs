module Main where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Parser
import Extras

data JSON
  = JSONString String
  | JSONNumber Double
  deriving Show

json :: Parser JSON
json =
      JSONString <$> stringLiteral
  <|> JSONNumber <$> numberLiteral

-- NUM_LIT
-- Integer numbers only.
numberLiteral :: Parser Double
numberLiteral =
  parse
  where
    sign    = let minus = satisfy (== '-') in
              fromMaybe 1 . fmap (const (-1)) <$> optional minus
    zero    = satisfy (== '0')
    nonZero = satisfy $ \c -> c >= '1' && c <= '9'
    digit   = satisfy $ \c -> c >= '0' && c <= '9'
    parse   = do
      sign'   <- sign
      digits  <-  (:[]) <$> zero
              <|> (:)   <$> nonZero <*> many digit
      return $ sign' * read digits

-- STR_LIT = QUOTE STR_LIT_BODY QUOTE
stringLiteral :: Parser String
stringLiteral = do
  quote *> stringLiteralBody <* quote
  where
    quote = satisfy (== '"')

-- STR_LIT_BODY
-- Escape sequences are not supported.
stringLiteralBody :: Parser String
stringLiteralBody =
  many $ satisfy isStringLiteralChar
  where
    isStringLiteralChar :: Char -> Bool
    isStringLiteralChar c = c /= '"' && c /= '\\' && c > '\x1F'

main :: IO ()
main = do
  s <- getLine
  print $ parse json s
