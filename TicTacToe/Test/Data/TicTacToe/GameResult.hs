module Test.Data.TicTacToe.GameResult
(
  main
, gameResultTests
) where

import Data.TicTacToe.GameResult
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Data.TicTacToe.Player()

instance Arbitrary GameResult where
  arbitrary = fmap (maybe draw win) arbitrary

main ::
  IO ()
main =
  defaultMain gameResultTests

gameResultTests ::
  [Test]
gameResultTests =
  [
    testGroup "GameResult"
      [
        testProperty "cata_ctor" prop_cata_ctor
      ]
  ]

prop_cata_ctor ::
  GameResult
  -> Bool
prop_cata_ctor r =
  gameResult win draw r == r
