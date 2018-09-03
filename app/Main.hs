module Main where

import Parser

stringLiteral :: Parser String
stringLiteral =
  \s ->
    case (combine (combine quote stringLliteralBody) quote) s of
      Just (((_, body), _), s') -> Just (body, s')
      Nothing                   -> Nothing
  where
    quote = satisfy (== '"')

stringLliteralBody :: Parser String
stringLliteralBody = many $ satisfy isStringLiteralChar

isStringLiteralChar :: Char -> Bool
isStringLiteralChar c =
  c /= '"' && c /= '\\' && c > '\x1F'

main :: IO ()
main = do
  s <- getLine
  print $ runParser stringLiteral s
