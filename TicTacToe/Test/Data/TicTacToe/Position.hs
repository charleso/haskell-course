module Test.Data.TicTacToe.Position where

import Data.TicTacToe.Position
import Test.QuickCheck

instance Arbitrary Position where
  arbitrary = elements [minBound ..]

