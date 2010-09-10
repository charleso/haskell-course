module L05.Testing where

import Prelude hiding (sum, length, map, filter, maximum, reverse)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import L02.List


-- How to produce arbitrary instances of List
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap (foldr (:|) Nil) arbitrary

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "List"
      [
        testProperty "map"            prop_map
      , testProperty "append_reverse" prop_append_reverse
      , testProperty "append"         prop_append
      , testProperty "foldRight"      prop_foldRight
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
  (x `append` y) `append` z == ---
  x `append` (y `append` z)   ---  error "todo"

-- Exercise 2
-- Folding (right) with cons and nil on a list (x) produces that same list x.
prop_foldRight ::
  List Int
  -> Bool
prop_foldRight x =
  foldRight (:|) Nil x == x --  error "todo"

