module L06.MoreParser where

import L03.Parser
import Control.Applicative

instance Functor Parser where
  fmap f p = bindParser p (valueParser . f)

instance Applicative Parser where
  pure = valueParser
  f <*> a = bindParser f (\f' ->
            bindParser a (\a' ->
            valueParser (f' a')))

instance Monad Parser where
  return = pure
  (>>=) = bindParser

thenSpaces ::
  Parser a
  -> Parser a
thenSpaces p =
  do v <- p
     spaces
     return v

commaThenSpaces ::
  Parser Char
commaThenSpaces =
  thenSpaces (pure ',')
