module L04.Fluffy where

import Prelude hiding (sum, length, map, filter, maximum, reverse)
import L01.Optional
import L01.Validation
import L02.List
import L03.Parser

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy List where
--  furry f Nil = Nil
--  furry f (x :| xs) = (f x) :| (furry f xs)
   furry = L02.List.map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Optional where
--   furry f Empty = Empty
--   furry f (Full a) = Full (f a)
    furry = L01.Optional.mapOptional

-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Parser where
--bindParser :: Parser a -> (a -> Parser b) -> Parser b
--furry :: (a -> b) -> Parser a -> Parser b
  furry f = (flip bindParser (valueParser . f))

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fluffy [] where
  furry = fmap
