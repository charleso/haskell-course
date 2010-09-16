{-# LANGUAGE TypeOperators #-}

module L05.Testing where

import Prelude hiding (sum, length, map, all, filter, maximum, reverse)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import L02.List
import Debug.Trace

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
  (append (append x y) z) == (append x (append y z))  

-- Exercise 2
-- Folding (right) with cons and nil on a list (x) produces that same list x.
prop_foldRight ::
  List Int
  -> Bool
prop_foldRight x =
  foldRight (:|) Nil x == x

prop_sum ::
  List Int
  -> Bool
prop_sum x =
  foldRight (+) 0 x == sum x

prop_length ::
  List Int
  -> Bool
prop_length x =
  foldRight (\_ y -> 1 + y) 0 x == length x

prop_filter ::
  (Int -> Bool)
  -> List Int
  -> Bool
prop_filter f x =
  all f (filter f x)

prop_map_composition ::
  (Int -> String)
  -> (Char -> Int)
  -> List Char
  -> Bool
prop_map_composition f g x =
  map f (map g x) == map (f . g) x

prop_flatten ::
  List (List Int)
  -> Bool
prop_flatten x =
  length (flatten x) == sum (map length x)

prop_flatMap_associative ::
  (Int -> List String)
  -> (String -> List Char)
  -> (Char -> List Integer)
  -> Int
  -> Bool
prop_flatMap_associative x y z a =
  let p >>>=> q = \k -> q `flatMap` p k
   in ((x >>>=> y)  >>>=> z) a == (x >>>=> (y >>>=> z)) a 
  


prop_maximum ::
  List Int
  -> Property
prop_maximum x =
  (not (isEmpty x)) ==>
  (all (\y -> y <= maximum x) x)

prop_reverse ::
  List Int
  -> Bool
prop_reverse x =
  reverse (reverse x) == x

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

