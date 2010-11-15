module L03.Parser where

import Data.Char
import L01.Validation
import L03.Person


type Input = String

data Parser a = P {
  parse :: Input -> Validation (Input, a)
}

-- Exercise 1
-- Return a parser that always succeeds
-- with the given value and consumes no input.
valueParser :: a -> Parser a
valueParser v = P (\r -> Value (r, v)) 

-- Exercise 2
-- Return a parser that always fails
-- with the given error.
failed :: Err -> Parser a
failed err = P (\_ -> Error err)

-- Exercise 3
-- Return a parser that succeeds with a character
-- off the input or fails with an error if the input is empty.
character :: Parser Char
character = P (\x -> case x of
                        []    -> Error "End of input"
                        (h:t) -> Value (t, h))

-- Exercise 4
-- Return a parser that puts its input into the given parser and
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--   * if that parser fails with an error the returned parser fails with that error.
bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser p f = P (\x -> case parse p x of 
                            Error msg    -> Error msg
                            Value (r, v) -> parse (f v) r
                   )

myLiftM2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
myLiftM2 f p1 p2 = bindParser p1 (\v1 -> bindParser p2 (\v2 -> valueParser (f v1 v2)))

myLiftM3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
myLiftM3 f p1 p2 p3 = bindParser p1 (\v1 -> 
                        bindParser p2 (\v2 ->
                          bindParser p3 (\v3 -> valueParser (f v1 v2 v3))))

-- Exercise 5
-- Return a parser that puts its input into the given parser and
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--   * if that parser fails with an error the returned parser fails with that error.
-- ~~~ This function should call bindParser. ~~~
(>>>) :: Parser a -> Parser b -> Parser b
(>>>) p1 p2 = bindParser p1 (\_ -> p2)

-- Exercise 6
-- Return a parser that tries the first parser for a successful value.
--   * If the first parser succeeds then use this parser.
--   * If the first parser fails, try the second parser.
(|||) :: Parser a -> Parser a -> Parser a
(|||) p1 p2 = P (\x -> case parse p1 x of
                         Error _      -> parse p2 x
                         Value (r, v) -> Value (r, v))

infixl 3 |||

-- Exercise 7
-- Return a parser that continues producing a list of values from the given parser.
-- ~~~ Use many1, valueParser and (|||). ~~~
list :: Parser a -> Parser [a]
list p = many1 p ||| valueParser []

-- Exercise 8
-- Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
-- The returned parser fails if
--   * The input is empty
-- ~~~ Use bindParser, list and value. ~~~
many1 :: Parser a -> Parser [a]
many1 p = myLiftM2 (:) p (list p)

-- Exercise 9
-- Return a parser that produces a character but fails if
--   * The input is empty.
--   * The character does not satisfy the given predicate.
-- ~~~ The bindParser and character functions will be helpful here. ~~~
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P (\i -> case i of
                       []    -> Error "End of input"
                       (h:_) -> if (f h) then parse character i else Error "Invalid character"
              )

-- Exercise 10.1
-- Return a parser that produces the given character but fails if
--   * The input is empty.
--   * The produced character is not equal to the given character.
-- ~~~ Use the satisfy function. ~~~
is :: Char -> Parser Char
is c' = satisfy (\c -> c == c')

-- Exercise 10.2
-- Return a parser that produces a character between '0' and '9' but fails if
--   * The input is empty.
--   * The produced character is not a digit.
-- ~~~ Use the satisfy and Data.Char.isDigit functions. ~~~
digit :: Parser Char
digit = satisfy Data.Char.isDigit

-- Exercise 10.3
-- Return a parser that produces zero or a positive integer but fails if
--   * The input is empty.
--   * The input does not produce a value series of digits
-- ~~~ Use the bindParser, valueParser, list and digit functions. ~~~
natural :: Parser Int
natural = bindParser (list digit) (\v -> valueParser (foldl (\a b -> a * 10 + digitToInt b) 0 v))

-- Exercise 10.4
-- Return a parser that produces a space character but fails if
--   * The input is empty.
--   * The produced character is not a space.
-- ~~~ Use the satisfy and Data.Char.isSpace functions. ~~~
space :: Parser Char
space = is ' '

-- Exercise 10.5
-- Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--   * The input is empty.
--   * The first produced character is not a space.
-- ~~~ Use the many1 and space functions. ~~~
spaces1 :: Parser String
spaces1 = list space

-- Exercise 10.6
-- Return a parser that produces a lower-case character but fails if
--   * The input is empty.
--   * The produced character is not lower-case.
-- ~~~ Use the satisfy and Data.Char.isLower functions. ~~~
lower :: Parser Char
lower = satisfy Data.Char.isLower

-- Exercise 10.7
-- Return a parser that produces an upper-case character but fails if
--   * The input is empty.
--   * The produced character is not upper-case.
-- ~~~ Use the satisfy and Data.Char.isUpper functions. ~~~
upper :: Parser Char
upper = satisfy Data.Char.isUpper

-- Exercise 10.8
-- Return a parser that produces an alpha character but fails if
--   * The input is empty.
--   * The produced character is not alpha.
-- ~~~ Use the satisfy and Data.Char.isAlpha functions. ~~~
alpha :: Parser Char
alpha = satisfy Data.Char.isAlpha

-- Exercise 11
-- Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
-- ~~~ Use bindParser and value. ~~~
-- ~~~ Optionally use Prelude.foldr. If not, an explicit recursive call. ~~~
sequenceParser :: [Parser a] -> Parser [a] 
sequenceParser = foldr (myLiftM2 (:)) (valueParser [])

-- Exercise 12
-- Return a parser that produces the given number of values off the given parser.
-- This parser fails if
--   * The given parser fails in the attempt to produce the given number of values.
-- ~~~ Use sequenceParser and Prelude.replicate. ~~~
thisMany :: Int -> Parser a -> Parser [a]
thisMany n p = sequenceParser (replicate n p)

-- Exercise 13
-- Write a parser for Person.age.
-- * Age: positive integer
-- ~~~ Equivalent to natural. ~~~
ageParser :: Parser Int
ageParser = natural

-- Exercise 14
-- Write a parser for Person.firstName.
-- * First Name: non-empty string that starts with a capital letter
-- ~~~ Use bindParser, value, upper, list and lower. ~~~
firstNameParser :: Parser String
firstNameParser = myLiftM2 (:) upper (list lower)

-- Exercise 15
-- Write a parser for Person.surname.
-- * Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters
-- ~~~ Use bindParser, value, upper, thisMany, lower and list. ~~~
surnameParser :: Parser String
surnameParser = myLiftM3 (\u l l' -> [u] ++ l ++ l') upper (thisMany 5 lower) (list lower)

-- Exercise 16
-- Write a parser for Person.gender.
-- * Gender: character that must be 'm' or 'f'
  -- ~~~ Use is and (|||). ~~~
genderParser :: Parser Char
genderParser = is 'm' ||| is 'f'

-- Exercise 17
-- Write part of a parser for Person.phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
-- * Phone: string of digits, dots or hyphens ...
-- ~~~ Use list, digit, (|||) and is. ~~~
phoneBodyParser :: Parser String
phoneBodyParser = list (digit ||| is '.' ||| is '-')

-- Exercise 18
-- Write a parser for Person.phone.
-- * Phone: ... but must start with a digit and end with a hash (#)
-- ~~~ Use bindParser, value, digit, phoneBodyParser and is. ~~~
phoneParser :: Parser String
phoneParser = myLiftM3 (\v1 v2 _ -> (v1:v2)) digit phoneBodyParser (is '#')

-- Exercise 19
-- Write a parser for Person.
-- ~~~ Use bindParser, value, (>>>)
--         ageParser,
--         firstNameParser,
--         surnameParser,
--         genderParser,
--         phoneParser ~~~
personParser :: Parser Person
personParser = bindParser ageParser (\age -> 
                 bindParser (spaces1 >>> firstNameParser) (\fname ->
                   bindParser (spaces1 >>> surnameParser) (\sname ->
                     bindParser (spaces1 >>> genderParser) (\sex ->
                       bindParser (spaces1 >>> phoneParser) (\tel ->
                         valueParser (Person age fname sname sex tel)
                       )
                     )
                   )
                 )
               )

-- Exercise 20
-- Make sure all the tests pass!
runTests :: IO ()
runTests = let outcome input expected = let actual = parse personParser input
                                            isError (Error _) = True
                                            isError (Value _) = False
                                        in if isError expected && isError actual || expected == actual
                                              then "PASSED."
                                              else concat ["FAILED parsing ",
                                                           input,
                                                           ". Expected: ",
                                                           show expected,
                                                           " Actual: ",
                                                           show actual]
           in mapM_ (print . uncurry outcome) tests

tests :: [(String, Validation (Input, Person))]
tests = [
         -- no input
         ("",                                      Error []),

          -- Age must be a positive integer
         ("12x Fred Clarkson m 123-456.789#",      Error []),

         -- First name must start with a capital letter
         ("123 fred Clarkson m 123-456.789#",      Error []),

         -- Surname must have at least 5 characters following the first
         ("123 Fred Cla m 123-456.789#",           Error []),

         -- Surname must start with a capital letter
         ("123 Fred clarkson m 123-456.789#",      Error []),

         -- Gender must be 'm' or 'f'
         ("123 Fred Clarkson x 123-456.789#",      Error []),

         -- Phone number (body) must be digits, dots or hyphens
         ("123 Fred Clarkson m 1x3-456.789#",      Error []),

         -- Phone number must start with a digit
         ("123 Fred Clarkson m -123-456.789#",     Error []),

         -- Phone number must end with a hash
         ("123 Fred Clarkson m 123-456.789",       Error []),

         -- Success with no further input
         ("123 Fred Clarkson m 123-456.789#",      Value ([], Person 123 "Fred" "Clarkson" 'm' "123-456.789")),

         -- Success with further input
         ("123 Fred Clarkson m 123-456.789# rest", Value (" rest", Person 123 "Fred" "Clarkson" 'm' "123-456.789"))]
