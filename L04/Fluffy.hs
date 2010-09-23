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
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Optional where
  furry = mapOptional


-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Parser where
--  furry x fa = bindParser fa (\a -> (valueParser (x a)))
--  furry x fa = bindParser fa (valueParser . x)
--  furry x = flip bindParser (valueParser . x)
  furry = flip bindParser . (valueParser .)


--
-- SUPPORT LIBRARIES --
--

instance Fluffy [] where
  furry = fmap
