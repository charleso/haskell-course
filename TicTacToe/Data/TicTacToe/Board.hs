module Data.TicTacToe.Board
(
  Board
, FinishedBoard
, getResult
, empty
, whoseTurn
, whoseNotTurn
, MoveResult(..)
, (-->)
, (--->)
, MovesAttempt
, attemptResult
, attemptPositions
, noRemainAttempt
, movesAttempt
, (===>)
, play
, play'
, BoardLike(..)
) where

import Prelude hiding (any, all, concat)
import Data.TicTacToe.Position
import Data.TicTacToe.Player
import Data.TicTacToe.GameResult
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable
import Data.List(intercalate)
import Data.Maybe

data MoveResult =
  PositionAlreadyOccupied
  | KeepPlaying Board
  | GameFinished FinishedBoard
  deriving Eq

instance Show MoveResult where
  show PositionAlreadyOccupied = "*Position already occupied*"
  show (KeepPlaying b)         = concat ["{", show b, "}"]
  show (GameFinished b)        = concat ["{{", show b, "}}"]

data Board =
  Board [(Position, Player)] !(M.Map Position Player)
  deriving Eq

instance Show Board where
  show b@(Board _ m) =
    intercalate " " [showPositionMap m, "[", show (whoseTurn b), "to move ]"]

data FinishedBoard =
  FinishedBoard Board GameResult
  deriving Eq

getResult ::
  FinishedBoard
  -> GameResult
getResult (FinishedBoard _ r) =
  r

instance Show FinishedBoard where
  show (FinishedBoard (Board _ m) r) =
    let summary = gameResult (\p -> show p ++ " wins") "draw" r
    in intercalate " " [showPositionMap m, "[[", summary, "]]"]

empty ::
  Board
empty =
  Board [] M.empty

whoseTurn ::
  Board
  -> Player
whoseTurn (Board [] _) =
  player1
whoseTurn (Board ((_, q):_) _) =
  alternate q

whoseNotTurn ::
  Board
  -> Player
whoseNotTurn =
  alternate . whoseTurn

(-->) ::
  Position
  -> Board
  -> MoveResult
p --> b@(Board q m) =
  let w       = whoseTurn b
      (j, m') = M.insertLookupWithKey (\_ x _ -> x) p w m
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
      b'            = Board ((p, w):q) m'
  in maybe (if isWin
            then
              GameFinished (b' `FinishedBoard` win w)
            else
              if isDraw
              then
                GameFinished (b' `FinishedBoard` draw)
              else
                KeepPlaying b') (const PositionAlreadyOccupied) j

(===>) ::
  [Position]
  -> Board
  -> MoveResult
p ===> b =
  attemptResult (p ---> b)

data MovesAttempt =
  MovesAttempt [Position] MoveResult
  deriving Eq

attemptResult ::
  MovesAttempt
  -> MoveResult
attemptResult (MovesAttempt _ r) =
  r

attemptPositions ::
  MovesAttempt
  -> [Position]
attemptPositions (MovesAttempt k _) =
  k

noRemainAttempt ::
  MoveResult
  -> MovesAttempt
noRemainAttempt =
  movesAttempt []

movesAttempt ::
  [Position]
  -> MoveResult
  -> MovesAttempt
movesAttempt =
  MovesAttempt

(--->) ::
  [Position]
  -> Board
  -> MovesAttempt
[]    ---> b =
  noRemainAttempt (KeepPlaying b)
(h:t) ---> b =
  case h --> b
  of PositionAlreadyOccupied -> movesAttempt (h:t) PositionAlreadyOccupied
     KeepPlaying b'          -> t ---> b'
     GameFinished b'         -> movesAttempt t (GameFinished b')

play ::
  [Position]
  -> MovesAttempt
play p =
  p ---> empty

play' ::
  [Position]
  -> MoveResult
play' p =
  p ===> empty

class BoardLike b where
  moveBack ::
    b
    -> Maybe Board

  isEmpty ::
    b
    -> Bool

  occupiedPositions ::
    b
    -> S.Set Position

  isSubboardOf ::
    b
    -> b
    -> Bool

  isProperSubboardOf ::
    b
    -> b
    -> Bool

  playerAt ::
    b
    -> Position
    -> Maybe Player

  playerAtOr ::
    b
    -> Position
    -> Player
    -> Player
  playerAtOr b p q =
    q `fromMaybe` playerAt b p

  isOccupied ::
    b
    -> Position
    -> Bool
  isOccupied b p =
    isJust $ playerAt b p

  isNotOccupied ::
    b
    -> Position
    -> Bool
  isNotOccupied b p =
    not (isOccupied b p)

  isFull ::
    b
    -> Bool

  printBoard ::
    b
    -> IO ()

instance BoardLike Board where
  moveBack (Board [] _) =
    Nothing
  moveBack (Board ((p, _):t) m) =
    Just (Board t (p `M.delete` m))

  isEmpty (Board _ m) =
    M.null m

  occupiedPositions (Board _ m) =
    M.keysSet m

  isSubboardOf (Board _ m) (Board _ m') =
    m `M.isSubmapOf` m'

  isProperSubboardOf (Board _ m) (Board _ m') =
    m `M.isProperSubmapOf` m'

  playerAt (Board _ m) p =
    p `M.lookup` m

  isFull (Board z _) =
    length z == 9

  printBoard (Board _ m) =
    let z = ".===.===.===."
        pos' = pos m " "
        lines = [
                  z
                , concat ["| ", pos' NW, " | ", pos' N , " | ", pos' NE, " |"]
                , z
                , concat ["| ", pos' W , " | ", pos' C , " | ", pos' E , " |"]
                , z
                , concat ["| ", pos' SW, " | ", pos' S , " | ", pos' SE, " |"]
                , z
                ]
    in forM_ lines print

instance BoardLike FinishedBoard where
  moveBack (FinishedBoard (Board [] _) _) =
    Nothing
  moveBack (FinishedBoard (Board ((p, _):t) m) _) =
    Just (Board t (p `M.delete` m))

  isEmpty (FinishedBoard b _) =
    isEmpty b

  occupiedPositions (FinishedBoard b _) =
    occupiedPositions b

  isSubboardOf (FinishedBoard b _) (FinishedBoard b' _) =
    b `isSubboardOf` b'

  isProperSubboardOf (FinishedBoard b _) (FinishedBoard b' _) =
    b `isProperSubboardOf` b'

  playerAt (FinishedBoard b _) p =
    b `playerAt` p

  isFull (FinishedBoard b _) =
    isFull b

  printBoard (FinishedBoard b _) =
    print b

-- not exported

pos ::
  Ord k =>
  M.Map k Player
  -> String
  -> k
  -> String
pos m empty p =
  maybe empty (player "X" "O") (p `M.lookup` m)

showPositionMap ::
  M.Map Position Player
  -> String
showPositionMap m =
  let pos' = pos m "?"
  in concat [ ".=",  pos' NW, "=.=", pos' N , "=.=", pos' NE
            , "=.=", pos' W , "=.=", pos' C , "=.=", pos' E
            , "=.=", pos' SW, "=.=", pos' S , "=.=", pos' SE, "=."
            ]
