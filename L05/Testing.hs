{-# LANGUAGE TypeOperators #-}

module L05.Testing where

import Prelude hiding (sum, length, map, all, filter, maximum, reverse)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import L02.List


-- How to produce arbitrary instances of List
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap (foldr (:|) Nil) arbitrary

instance Show (a -> b) where
  show _ = "<function>"

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "List"
      [
        testProperty "map (identity)"           prop_map
      , testProperty "append_reverse"           prop_append_reverse
      , testProperty "append"                   prop_append
      , testProperty "foldRight"                prop_foldRight
      , testProperty "sum"                      prop_sum
      , testProperty "length"                   prop_length
      , testProperty "filter"                   prop_filter
      , testProperty "map (composition)"        prop_map_composition
      , testProperty "flatten"                  prop_flatten
      , testProperty "flatMap (associativity)"  prop_flatMap_associative
      , testProperty "maximum"                  prop_maximum
      , testProperty "reverse"                  prop_reverse
      ]
  ]

-- Mapping the identity function on any value (x) produces that value x.
prop_map ::
  List Int
  -> Bool
prop_map x =
  map id x == x

-- Appending x to y then reversing produces the same result as
-- reversing y and appending to the result of reversing x.
prop_append_reverse ::
  List Int
  -> List Int
  -> Bool
prop_append_reverse x y =
  reverse (append x y) == append (reverse y) (reverse x)

-- Exercise 1
-- Appending (x to y) to z produces the same result as
-- appending x to (y to z).
-- The law of associativity.
prop_append ::
  List Int
  -> List Int
  -> List Int
  -> Bool
prop_append x y z =
  error "todo"

-- Exercise 2
-- Folding (right) with cons and nil on a list (x) produces that same list x.
prop_foldRight ::
  List Int
  -> Bool
prop_foldRight x =
  error "todo"

prop_sum ::
  List Int
  -> Bool
prop_sum x =
  error "todo"

prop_length ::
  List Int
  -> Bool
prop_length x =
  error "todo"

prop_filter ::
  Int `Fun` Bool
  -> List Int
  -> Bool
prop_filter f x =
  let f' = apply f
  in error "todo"

prop_map_composition ::
  Int `Fun` String
  -> Char `Fun` Int
  -> List Char
  -> Bool
prop_map_composition f g x =
  let f' = apply f
      g' = apply g
  in error "todo"

prop_flatten ::
  List (List Int)
  -> Bool
prop_flatten x =
  error "todo"

prop_flatMap_associative ::
  (Int -> List String)
  -> (String -> List Char)
  -> (Char -> List Integer)
  -> Int
  -> Bool
prop_flatMap_associative x y z a =
  let p >>>=> q = \k -> q `flatMap` p k
  in error "todo"

prop_maximum ::
  List Int
  -> Property
prop_maximum x =
  error "todo"

prop_reverse ::
  List Int
  -> Bool
prop_reverse x =
  error "todo"

-- Utility

all ::
  (a -> Bool)
  -> List a
  -> Bool
all p =
  foldRight (&&) True . map p

isEmpty ::
  List a
  -> Bool
isEmpty Nil    = True
isEmpty (_:|_) = False

