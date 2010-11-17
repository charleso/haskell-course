module Data.TicTacToe.Board
(
  Board
, empty
, whoseTurn
, MoveResult(..) -- todo abstract ADT
, (-->)
, (--->)
) where

import Prelude hiding (any, all, concat)
import Data.TicTacToe.Position
import Data.TicTacToe.Player
import Data.TicTacToe.GameResult
import qualified Data.Map as M
import Data.Foldable
import Data.List(intercalate)

data MoveResult =
  PositionAlreadyOccupied
  | KeepPlaying Board
  | GameFinished FinishedBoard

instance Show MoveResult where
  show PositionAlreadyOccupied = "*Position already occupied*"
  show (KeepPlaying b)         = concat ["{", show b, "}"]
  show (GameFinished b)        = concat ["{{", show b, "}}"]

data Board =
  Board (M.Map Position Player) !Player
  deriving Eq

instance Show Board where
  show (Board m p) =
    intercalate " " [showPositionMap m, "[", show p, "to move ]"]

data FinishedBoard =
  FinishedBoard (M.Map Position Player) GameResult
  deriving Eq

instance Show FinishedBoard where
  show (FinishedBoard m r) =
    let summary = foldGameResult (\p -> show p ++ " wins") "draw"
    in intercalate " " [showPositionMap m, "[[", summary r, "]]"]

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

(-->) ::
  Position
  -> Board
  -> MoveResult
p --> (Board m w) =
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
      isWin         = any (\(a, b, c) -> any allEq $ mapM (`M.lookup` m') [a, b, c]) wins
      isDraw        = all (`M.member` m') [minBound ..]
  in maybe (if isWin
            then
              GameFinished (FinishedBoard m' (win w))
            else
              if isDraw
              then
                GameFinished (FinishedBoard m' draw)
              else
                KeepPlaying (Board m' (alternate w))) (const PositionAlreadyOccupied) j

(--->) ::
  [Position]
  -> Board
  -> ([Position], MoveResult)
[]    ---> b =
  ([], KeepPlaying b)
(h:t) ---> b =
  case h --> b
  of PositionAlreadyOccupied -> (h:t, PositionAlreadyOccupied)
     KeepPlaying b'          -> t ---> b'
     GameFinished b'         -> (t, GameFinished b')

-- not exported
showPositionMap ::
  M.Map Position Player
  -> String
showPositionMap m =
  let pos p = maybe "?" (player "X" "O") (p `M.lookup` m)
  in concat [ ".=",  pos NW, "=.=", pos N , "=.=", pos NE
            , "=.=", pos W , "=.=", pos C , "=.=", pos E
            , "=.=", pos SW, "=.=", pos S , "=.=", pos SE, "=."
            ]
