module Extras (
    satisfy
  ) where

import Parser

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  Parser $ \s ->
    case s of
      c:cs -> justIf (p c) (c, cs)
      []   -> Nothing

justIf :: Bool -> a -> Maybe a
justIf True  = Just
justIf False = const Nothing
