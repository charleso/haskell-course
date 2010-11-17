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
