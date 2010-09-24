module L06.MoreParser where

import L01.Validation
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

spaces :: Parser String
spaces =list space

tok ::
  Parser a
  -> Parser a
tok p =
  do v <- p
     spaces1
     return v

charTok ::
  Char
  -> Parser Char
charTok =
  tok . is

betweenCharTok ::
  Char
  -> Char
  -> Parser a
  -> Parser a
betweenCharTok a b =
  between (charTok a) (charTok b)

commaTok ::
  Parser Char
commaTok =
  charTok ','

quote ::
  Parser Char
quote =
  is '"' ||| is '\"'

string ::
  String
  -> Parser String
string =
  mapM is

stringTok ::
  String
  -> Parser String
stringTok =
  tok . string

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

sepby ::
  Parser a
  -> Parser s
  -> Parser [a]
sepby p s =
  sepby1 p s ||| return []

sepby1 ::
  Parser a
  -> Parser s
  -> Parser [a]
sepby1 p s =
  do v <- p
     w <- list (s >> p)
     return (v:w)

eof ::
  Parser ()
eof = P (\s -> case s of [] -> Value ([], ())
                         x -> Error ("Expected EOF but got " ++ x))

satisfyAll ::
  [Char -> Bool]
  -> Parser Char
satisfyAll ps =
  satisfy (and  . sequence ps)

satisfyAny ::
  [Char -> Bool]
  -> Parser Char
satisfyAny ps =
  satisfy (or  . sequence ps)

