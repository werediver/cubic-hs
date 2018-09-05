module Parser (
    Parser,
    runParser,
    satisfy,
    many,
    some,
    combine,
    combine3,
    alternative,
    optional,
    pmap
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

combine3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
combine3 p q r =
  \s ->
    case (p `combine` q `combine` r) s of
      Just (((a, b), c), s') -> Just ((a, b, c), s')
      Nothing          -> Nothing

alternative :: Parser a -> Parser a -> Parser a
alternative p q =
  \s ->
    case p s of
      Nothing -> q s
      smth    -> smth

optional :: Parser a -> Parser (Maybe a)
optional p =
  \s ->
    case p s of
      Nothing      -> Just (Nothing, s)
      Just (a, s') -> Just (Just a, s')

pmap :: Parser a -> (a -> b) -> Parser b
pmap p f =
  \s ->
    case p s of
      Just (a, s') -> Just(f a, s')
      Nothing      -> Nothing
