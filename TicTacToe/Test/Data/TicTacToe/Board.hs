module Test.Data.TicTacToe.Board
(
  main
, boardTests
) where

import Data.TicTacToe.Board
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

instance Arbitrary Board where
  arbitrary =
    undefined

main ::
  IO ()
main =
  defaultMain boardTests

boardTests ::
  [Test]
boardTests =
  [
    testGroup "Board"
      [
      -- todo
      ]
  ]
