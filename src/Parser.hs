module Parser (
    Parser,
    runParser,
    satisfy,
    many,
    some,
    combine
  ) where

type Parser a = String -> Maybe (a, String)

runParser :: Parser a -> String -> Maybe a
runParser p s =
  case p s of
    Just (a, _) -> Just a
    Nothing     -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
  \s ->
    case s of
      c:cs -> if f c then Just (c, cs) else Nothing
      []   -> Nothing

many :: Parser a -> Parser [a]
many p =
  \s ->
    Just $ scan [] s
  where
    scan as s =
      case p s of
        Just (a, s') -> scan (as ++ [a]) s'
        Nothing      -> (as, s)

some :: Parser a -> Parser [a]
some p =
  \s ->
    case p s of
      Just (a, s') ->
        case many p s' of
          Just (as, s'') -> Just (a:as, s'')
          Nothing        -> Nothing
      Nothing ->
        Nothing

combine :: Parser a -> Parser b -> Parser (a, b)
combine p q =
  \s ->
    case p s of
      Just (a, s') ->
        case q s' of
          Just (b, s'') -> Just ((a, b), s'')
          Nothing       -> Nothing
      Nothing ->
        Nothing
