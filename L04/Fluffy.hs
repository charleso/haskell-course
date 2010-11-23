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
  -- furry :: (a -> b) -> List a -> List b
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Optional where
  --furry :: (a -> b) -> Optional a -> Optional b
  furry _ Empty = Empty
  furry f (Full a) = Full (f a)

-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Parser where
  -- furry :: (a -> b) -> Parser a -> Parser b
  furry f p = bindParser p (\a -> valueParser (f a))


--
-- SUPPORT LIBRARIES --
--

instance Fluffy [] where
  furry = fmap
