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
      ]
  ]

prop_map :: List Int -> Bool
prop_map x = map id x == x

prop_append_reverse :: List Int -> List Int -> Bool
prop_append_reverse x y = reverse (append x y) == append (reverse y) (reverse x)

