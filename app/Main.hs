module Main where

import Control.Applicative
import Parser
import Extras

-- STR_LIT = QUOTE STR_LIT_BODY QUOTE
stringLiteral :: Parser String
stringLiteral = do
  _ <- quote
  body <- stringLiteralBody
  _ <- quote
  return body
  where
    quote = satisfy (== '"')

-- STR_LIT_BODY
stringLiteralBody :: Parser String
stringLiteralBody =
    many $ satisfy isStringLiteralChar
  where
    isStringLiteralChar :: Char -> Bool
    isStringLiteralChar c = c /= '"' && c /= '\\' && c > '\x1F'

main :: IO ()
main = do
  s <- getLine
  print $ parse stringLiteral s
