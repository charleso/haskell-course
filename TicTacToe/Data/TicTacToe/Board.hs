module Data.TicTacToe.Board
(
  Board
, empty
, whoseTurn
, GameResult(..) -- todo abstract ADT
, MoveResult(..) -- todo abstract ADT
, move
) where

import Prelude hiding (any, all)
import Data.TicTacToe.Position
import Data.TicTacToe.Player
import qualified Data.Map as M
import Data.Foldable

data GameResult =
  Win Player
  | Draw
  | NoResult
  deriving Eq

data MoveResult =
  PositionAlreadyOccupied
  | GameFinished FinishedBoard -- todo do not use a Board, but instead a Board for which there is no move
  | KeepPlaying Board

data Board =
  Board (M.Map Position Player) !Player
  deriving Eq

data FinishedBoard =
  FinishedBoard (M.Map Position Player) GameResult
  deriving Eq

-- Board only (not FinishedBoard)
empty ::
  Board
empty =
  Board M.empty player1

whoseTurn ::
  Board
  -> Player
whoseTurn (Board _ p) =
  p

move ::
  Position
  -> Board
  -> MoveResult
move p (Board m w) =
  let (j, m') = M.insertLookupWithKey (\_ x _ -> x) p w m
      wins =
        [
          (NW, W , SW)
        , (N , C , S )
        , (NE, E , SE)
        , (NW, N , NE)
        , (W , C , E )
        , (SW, S , SE)
        , (NW, C , SE)
        , (SW, C , NE)
        ]
      allEq (d:e:t) = d == e && allEq (e:t)
      allEq _       = True
      isWin         = any (\(a, b, c) -> any allEq $ mapM (`M.lookup` m) [a, b, c]) wins
      isDraw        = all (`M.member` m') [minBound ..]
  in maybe (if isWin
            then
              GameFinished (FinishedBoard m' (Win w))
            else
              if isDraw
              then
                GameFinished (FinishedBoard m' Draw)
              else
                KeepPlaying (Board m' (alternate p))) (const PositionAlreadyOccupied) j

-- not exported
