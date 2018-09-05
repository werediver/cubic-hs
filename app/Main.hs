module Main where

import Parser

data JSON
  = JSONString String
  | JSONNumber Double
  deriving Show

json :: Parser JSON
json =
  alternative jsonString jsonNumber
  where
    jsonString = pmap stringLiteral JSONString
    jsonNumber = pmap numberLiteral JSONNumber

numberLiteral :: Parser Double
numberLiteral =
  \s ->
    case parse s of
      Just ((sign, cs), s') -> Just (sign * read cs :: Double, s')
      Nothing               -> Nothing
  where
    sign    = let minus = satisfy (== '-') in
              pmap (optional minus) $ \mc ->
                case mc of
                  Just _  -> -1
                  Nothing -> 1
    zero    = satisfy (== '0')
    nonZero = satisfy (\c -> c >= '1' && c <= '9')
    digit   = satisfy (\c -> c >= '0' && c <= '9')
    parse   = combine
      sign
      (alternative
        (pmap zero (:[]))
        (pmap (combine nonZero (many digit)) (\(c, cs) -> c:cs)))

stringLiteral :: Parser String
stringLiteral =
  \s ->
    case parse s of
      Just ((_, body, _), s') -> Just (body, s')
      Nothing                 -> Nothing
  where
    quote = satisfy (== '"')
    parse = combine3 quote stringLliteralBody quote

stringLliteralBody :: Parser String
stringLliteralBody = many $ satisfy isStringLiteralChar

isStringLiteralChar :: Char -> Bool
isStringLiteralChar c =
  c /= '"' && c /= '\\' && c > '\x1F'

main :: IO ()
main = do
  s <- getLine
  print $ runParser json s
