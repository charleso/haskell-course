module Test.Data.TicTacToe.Board
(
  main
, boardTests
) where

import Prelude hiding (all, any)
import Data.TicTacToe.Board
import Data.TicTacToe.Position
import Data.Foldable
import Test.Data.TicTacToe.Position()
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

instance Arbitrary Board where
  arbitrary =
    filterEndMoves empty `fmap` resize 12 arbitrary

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
        testProperty "whoseTurn"       prop_whoseTurn
      , testProperty "move_whoseTurn"  prop_move_whoseTurn
      , testProperty "move_moveBack"   prop_move_moveBack
      ]
  ]

prop_whoseTurn ::
  Board
  -> Bool
prop_whoseTurn b =
  whoseTurn b /= whoseNotTurn b

prop_move_whoseTurn ::
  Board
  -> Position
  -> Bool
prop_move_whoseTurn b p =
  (\b' -> whoseTurn b /= whoseTurn b') `all` boardResult (p --> b)

prop_move_moveBack ::
  Board
  -> Position
  -> Bool
prop_move_moveBack b p =
  (\b' -> (== b) `all` (moveBack b')) `all` boardResult (p --> b)

