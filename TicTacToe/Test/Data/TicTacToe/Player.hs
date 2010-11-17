module Test.Data.TicTacToe.Player where

import Data.TicTacToe.Player
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

instance Arbitrary Player where
  arbitrary = elements [player1, player2]


main ::
  IO ()
main =
  defaultMain playerTests

playerTests ::
  [Test]
playerTests =
  [
    testGroup "Player"
      [
        testProperty "is_exclusive" prop_is_exclusive
      ]
  ]

prop_is_exclusive ::
  Player
  -> Bool
prop_is_exclusive p =
  isPlayer1 p /= isPlayer2 p

{-
prop_neHead :: String
               -> [String]
               -> Bool
prop_neHead h t = neHead (nonEmpty h t) == h

prop_neTail :: String
               -> [String]
               -> Bool
prop_neTail h t = neTail (nonEmpty h t) == t

prop_nonEmpty :: String
                 -> [String]
                 -> Bool
prop_nonEmpty h t = toList (nonEmpty h t) == h:t

prop_nonEmptyAlias :: String
                      -> [String]
                      -> Bool
prop_nonEmptyAlias h t = nonEmpty h t == h |: t

prop_toNonEmpty :: [String]
                  -> Bool
prop_toNonEmpty x = toNonEmpty x == case x of [] -> Nothing
                                              (h:t) -> Just (nonEmpty h t)

prop_unsafeToNonEmpty :: [String]
                       -> Property
prop_unsafeToNonEmpty x = not (null x) ==> unsafeToNonEmpty x == nonEmpty (head x) (tail x)

prop_cons :: String
             -> NonEmpty String
             -> Bool
prop_cons a as = toList (a .: as) == a : toList as

prop_append :: NonEmpty String
               -> NonEmpty String
               -> Bool
prop_append a b = toList (a .++. b) == neHead a : neTail a ++ neHead b : neTail b

prop_reverse :: NonEmpty String
                -> Bool
prop_reverse x = (reverse . reverse) x == x
-}
