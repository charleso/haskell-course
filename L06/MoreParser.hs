module L06.MoreParser where

import L03.Parser
import Data.Char
import Numeric
import Control.Applicative
import Control.Monad

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

quote ::
  Parser Char
quote =
  is '"'

string ::
  String
  -> Parser String
string =
  mapM is

stringThenSpaces ::
  String
  -> Parser String
stringThenSpaces =
  thenSpaces . string

option ::
  a
  -> Parser a
  -> Parser a
option a p =
  p ||| return a

digits ::
  Parser String
digits =
  many1 digit

oneof ::
  String
  -> Parser Char
oneof s =
  satisfy (flip elem s)

noneof ::
  String
  -> Parser Char
noneof s =
  satisfy (flip notElem s)

between ::
  Parser o
  -> Parser c
  -> Parser a
  -> Parser a
between o c a =
  do o
     v <- a
     c
     return v

hex ::
  Parser Char
hex =
  let hInt s = case readHex s
               of [] -> 0
                  ((n, _):_) -> n
  in do is 'u'
        h <- replicateM 4 (satisfy isHexDigit)
        return . chr . hInt $ h

