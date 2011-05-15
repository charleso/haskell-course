module L06.MoreParser where

import L01.Validation
import L03.Parser
import Data.Char
import Numeric
import Control.Applicative
import Control.Monad

-- Parses the given input and returns the result.
-- The remaining input is ignored.
(<.>) ::
  Parser a
  -> Input
  -> Validation a
(<.>) i =
  mapValidation snd . parse i

-- Exercise 1
-- Write a Functor instance for a Parser.
-- ~~~ Use bindParser and valueParser ~~~
instance Functor Parser where
  fmap f p =
    bindParser p (valueParser . f)

-- Exercise 2
-- Write an Applicative functor instance for a Parser.
-- ~~~ Use bindParser and valueParser ~~~
instance Applicative Parser where
  pure =
    valueParser
  f <*> a =
    -- bindParser f (\f' -> fmap f' a)
    bindParser f (flip fmap a)

-- Exercise 3
-- Write a Monad instance for a Parser.
instance Monad Parser where
  return =
    valueParser
  (>>=) =
    bindParser

-- Exercise 4
-- Read documentation, ask questions.
{-

Check out the libraries now available to Parsers as a result of the Applicative and Monad instances.
4.1 http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Applicative.html
4.2 http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Monad.html
4.3 http://haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Data-Traversable.html

4.4 Learn about do-notation, now that we have just written a Monad instance
4.4.1 http://en.wikibooks.org/wiki/Haskell/do_Notation
4.4.2 http://www.haskell.org/onlinereport/exps.html#sect3.14

We will gently start using these libraries. Identify the pattern of computation in the problems below.

-}

-- Exercise 5
-- Write a parser that will parse zero or more spaces.
spaces ::
  Parser String
spaces =
  error "todo"

-- Exercise 6
-- Write a function that applies the given parser, then parses 0 or more spaces,
-- then produces the result of the original parser.
-- ~~~ Use the monad instance ~~~
tok ::
  Parser a
  -> Parser a
tok p =
  error "todo"

-- Exercise 7
-- Write a function that parses the given char followed by 0 or more spaces.
-- ~~~ Use tok and is ~~~
charTok ::
  Char
  -> Parser Char
charTok =
  error "todo"

-- Exercise 8
-- Write a parser that parses a comma ',' followed by 0 or more spaces.
-- ~~~ Use charTok ~~~
commaTok ::
  Parser Char
commaTok =
  error "todo"

-- Exercise 9
-- Write a parser that parses either a double-quote or a single-quote.
-- ~~~ Use is and (|||) ~~~
quote ::
  Parser Char
quote =
  error "todo"

-- Exercise 10
-- Write a function that parses the given string (fails otherwise).
-- ~~~ Use is and mapM ~~~
string ::
  String
  -> Parser String
string =
  error "todo"

-- Exercise 11
-- Write a function that parsers the given string, followed by 0 or more spaces.
-- ~~~ Use tok and string ~~~
stringTok ::
  String
  -> Parser String
stringTok =
  error "todo"

-- Exercise 12
-- Write a function that tries the given parser, otherwise succeeds by producing the given value.
-- ~~~ Use (|||) ~~~
option ::
  a
  -> Parser a
  -> Parser a
option a p =
  error "todo"

-- Exercise 13
-- Write a parser that parses 1 or more digits.
-- ~~~ Use many1 and digit ~~~
digits1 ::
  Parser String
digits1 =
  error "todo"

-- Exercise 14
-- Write a function that parses one of the characters in the given string.
-- ~~~ Use satisfy and elem ~~~
oneof ::
  String
  -> Parser Char
oneof s =
  error "todo"

-- Exercise 15
-- Write a function that parses any character, but fails if it is in the given string.
-- ~~~ Use satisfy and notElem ~~~
noneof ::
  String
  -> Parser Char
noneof s =
  error "todo"

-- Exercise 16
-- Write a function that applies the first parser, runs the second parser keeping the result,
-- then runs the third parser and produces the obtained result.
-- ~~~ Use the Monad instance ~~~
between ::
  Parser o
  -> Parser c
  -> Parser a
  -> Parser a
between o c a =
  error "todo"

-- Exercise 17
-- Write a function that applies the given parser in between the two given characters.
-- ~~~ Use between and charTok ~~~
betweenCharTok ::
  Char
  -> Char
  -> Parser a
  -> Parser a
betweenCharTok a b =
  error "todo"

-- Exercise 18
-- Write a function that parses the character 'u' followed by 4 hex digits and return the character value.
-- ~~~ Use readHex, isHexDigit, replicateM, satisfy and the Monad instance ~~~
hex ::
  Parser Char
hex =
  error "todo"

-- Exercise 19
-- Write a function that produces a non-empty list of values coming off the given parser (which must succeed at least once),
-- separated by the second given parser.
-- ~~~ Use list and the Monad instance ~~~
sepby1 ::
  Parser a
  -> Parser s
  -> Parser [a]
sepby1 p s =
  error "todo"

-- Exercise 20
-- Write a function that produces a list of values coming off the given parser,
-- separated by the second given parser.
-- ~~~ Use sepby1 and (|||) ~~~
sepby ::
  Parser a
  -> Parser s
  -> Parser [a]
sepby p s =
  error "todo"

-- Exercise 21
-- Write a parser that asserts that there is no remaining input.
eof ::
  Parser ()
eof =
  error "todo"

-- Exercise 22
-- Write a parser that produces a characer that satisfies all of the given predicates.
-- ~~~ Use sequence and Data.List.and ~~~
satisfyAll ::
  [Char -> Bool]
  -> Parser Char
satisfyAll ps =
  error "todo"

-- Exercise 23
-- Write a parser that produces a characer that satisfies any of the given predicates.
-- ~~~ Use sequence and Data.List.or ~~~
satisfyAny ::
  [Char -> Bool]
  -> Parser Char
satisfyAny ps =
  error "todo"

-- Exercise 24
-- Write a parser that parses between the two given characters, separated by a comma character ','.
-- ~~~ Use betweenCharTok, sepby and charTok ~~~
betweenSepbyComma ::
  Char
  -> Char
  -> Parser a
  -> Parser [a]
betweenSepbyComma a b g =
  error "todo"
