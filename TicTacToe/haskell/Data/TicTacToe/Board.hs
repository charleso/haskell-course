-- | A tic-tac-toe board is one of nine positions, each position occupied by either player 1, player 2 or neither and with invariants specific to the rules of tic-tac-toe.
--
-- For example, the number of positions occupied by player 1 is equal to, or one more, than the positions occupied by player 2.
module Data.TicTacToe.Board
(
-- * Board data types
  Board
, FinishedBoard
-- * Start new game
, empty
-- * Game completed
, getResult
-- * Which player to move
, whoseTurn
, whoseNotTurn
-- * Make a move on a board
, MoveResult
, foldMoveResult
, boardResult
, boardResultOr
, (-->)
-- * Make many moves on a board
, MovesAttempt
, attemptResult
, attemptPositions
, noRemainAttempt
, movesAttempt
, (--->)
, (===>)
, play
, play'
, (-?->)
, playAny
-- * Operations common to boards in-play and completed
, BoardLike(..)
-- * Debugging
, printEachPosition
) where

import Prelude hiding (any, all, concat, foldr)
import Data.TicTacToe.Position
import Data.TicTacToe.Player
import Data.TicTacToe.GameResult
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable
import Data.List(intercalate)
import Data.Maybe

-- | The result of making a move on a tic-tac-toe board.
data MoveResult =
  PositionAlreadyOccupied -- ^ The move was to a position that is already occupied by a player.
  | KeepPlaying Board -- ^ The move was valid and the board is in a new state.
  | GameFinished FinishedBoard -- ^ The move was valid and the game is complete.
  deriving Eq

-- | Deconstruct a move result.
foldMoveResult ::
  a -- ^ The move was to a position that is already occupied by a player.
  -> (Board -> a) -- ^ The move was valid and the board is in a new state.
  -> (FinishedBoard -> a) -- ^ The move was valid and the game is complete.
  -> MoveResult
  -> a
foldMoveResult occ _ _ PositionAlreadyOccupied =
  occ
foldMoveResult _ kp _ (KeepPlaying b) =
  kp b
foldMoveResult _ _ gf (GameFinished b) =
  gf b

-- | Return the possible board from a move result. A board is returned if the result is to continue play.
boardResult ::
  MoveResult
  -> Maybe Board
boardResult (KeepPlaying b) =
  Just b
boardResult _               =
  Nothing

-- | Return the board from a move result or if the default if there isn't one.
boardResultOr ::
  Board -- ^ The default.
  -> MoveResult -- ^ The move result to get the board from.
  -> Board
boardResultOr b =
  fromMaybe b . boardResult

instance Show MoveResult where
  show PositionAlreadyOccupied = "*Position already occupied*"
  show (KeepPlaying b)         = concat ["{", show b, "}"]
  show (GameFinished b)        = concat ["{{", show b, "}}"]

-- | A tic-tac-toe board.
data Board =
  Board [(Position, Player)] !(M.Map Position Player)
  deriving Eq

instance Show Board where
  show b@(Board _ m) =
    intercalate " " [showPositionMap m, "[", show (whoseTurn b), "to move ]"]

-- | A finished board is a completed tic-tac-toe game and does not accept any more moves.
data FinishedBoard =
  FinishedBoard Board GameResult
  deriving Eq

-- | Return the result of a completed tic-tac-toe game.
getResult ::
  FinishedBoard
  -> GameResult
getResult (FinishedBoard _ r) =
  r

instance Show FinishedBoard where
  show (FinishedBoard (Board _ m) r) =
    let summary = gameResult (\p -> show p ++ " wins") "draw" r
    in intercalate " " [showPositionMap m, "[[", summary, "]]"]

-- | Start an empty tic-tac-toe board.
empty ::
  Board
empty =
  Board [] M.empty

-- | Returns whose turn it is on a tic-tac-toe board.
whoseTurn ::
  Board
  -> Player
whoseTurn (Board [] _) =
  player1
whoseTurn (Board ((_, q):_) _) =
  alternate q

-- | Returns whose turn it is not on a tic-tac-toe board.
whoseNotTurn ::
  Board
  -> Player
whoseNotTurn =
  alternate . whoseTurn

-- | Make a move at the given position on the given board.
(-->) ::
  Position -- ^ The position to move to.
  -> Board -- ^ The board to make the move on.
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

-- | The result of attempting to make several moves on a board.
--
-- Encapsulates the last successful move result and any remaining positions that could not be attempted.
data MovesAttempt =
  MovesAttempt [Position] MoveResult
  deriving Eq

-- | Get the move result from the move attempts.
attemptResult ::
  MovesAttempt
  -> MoveResult
attemptResult (MovesAttempt _ r) =
  r

-- | Get the remaining positions from the move attempts.
attemptPositions ::
  MovesAttempt
  -> [Position]
attemptPositions (MovesAttempt k _) =
  k

-- | Construct a moves attempt with no remaining positions and the given move result.
noRemainAttempt ::
  MoveResult
  -> MovesAttempt
noRemainAttempt =
  movesAttempt []

-- | Construct a moves attempt with the given remaining positions and move result.
movesAttempt ::
  [Position]
  -> MoveResult
  -> MovesAttempt
movesAttempt =
  MovesAttempt

-- | Attempt to make several moves on a board.
--
-- If a move is encountered that does not transition to a new board state (e.g. game is finished, or the position is occupied),
-- then that result is returned along with the remaining moves.
(--->) ::
  [Position] -- ^ The moves to make.
  -> Board
  -> MovesAttempt
[]    ---> b =
  noRemainAttempt (KeepPlaying b)
(h:t) ---> b =
  case h --> b
  of PositionAlreadyOccupied -> movesAttempt (h:t) PositionAlreadyOccupied
     KeepPlaying b'          -> t ---> b'
     GameFinished b'         -> movesAttempt t (GameFinished b')

-- | Attempt to make several moves on a board.
--
-- If a move is encountered that does not transition to a new board state (e.g. game is finished, or the position is occupied),
-- then that result is returned.
(===>) ::
  [Position] -- ^ The moves to make.
  -> Board
  -> MoveResult
p ===> b =
  attemptResult (p ---> b)

-- | Attempt to make several moves on an empty board.
--
-- If a move is encountered that does not transition to a new board state (e.g. game is finished, or the position is occupied),
-- then that result is returned along with the remaining moves.
play ::
  [Position] -- ^ The moves to make.
  -> MovesAttempt
play p =
  p ---> empty

-- | Attempt to make several moves on an empty board.
--
-- If a move is encountered that does not transition to a new board state (e.g. game is finished, or the position is occupied),
-- then that result is returned.
play' ::
  [Position] -- ^ The moves to make.
  -> MoveResult
play' p =
  p ===> empty

-- | Make several moves on a board, discarding those that do not result in a new board state (e.g. game is finished, or the position is occupied)
(-?->) ::
  Board
  -> [Position] -- ^ The moves to make.
  -> Board
(-?->) =
  foldr (\p b -> case p --> b
                 of KeepPlaying b' -> b'
                    _              -> b)

-- | Make several moves on a new board, discarding those that do not result in a new board state (e.g. game is finished, or the position is occupied)
playAny ::
  [Position] -- ^ The moves to make.
  -> Board
playAny =
  (-?->) empty

-- | Prints out a board using ASCII notation and substituting the returned string for each position.
printEachPosition ::
  (Position -> String) -- ^ The function returning the string to substitute each position.
  -> IO ()
printEachPosition k =
  let z = ".===.===.===."
      lines = [
                z
              , concat ["| ", k NW, " | ", k N , " | ", k NE, " |"]
              , z
              , concat ["| ", k W , " | ", k C , " | ", k E , " |"]
              , z
              , concat ["| ", k SW, " | ", k S , " | ", k SE, " |"]
              , z
              ]
  in forM_ lines putStrLn

-- | Functions that work on boards that are in play or have completed.
--
-- This class specifically does not specify moving on a board, since this is illegal on a completed board.
class BoardLike b where
  -- | Takes a move back, unless the board is empty.
  moveBack ::
    b
    -> Maybe Board

  -- | Returns whether or not the board is empty.
  isEmpty ::
    b
    -> Bool

  -- | Returns positions that are occupied.
  occupiedPositions ::
    b
    -> S.Set Position

  -- | Returns the number of moves that have been played.
  moves ::
    b
    -> Int

  -- | Returns whether or not the first given board can transition to the second given board.
  isSubboardOf ::
    b
    -> b
    -> Bool

  -- | Returns whether or not the first given board can transition to the second given board and they are inequal.
  isProperSubboardOf ::
    b
    -> b
    -> Bool

  -- | Returns the player at the given position.
  playerAt ::
    b
    -> Position
    -> Maybe Player

  -- | Returns the player at the given position or the given default.
  playerAtOr ::
    b
    -> Position
    -> Player
    -> Player
  playerAtOr b p q =
    q `fromMaybe` playerAt b p

  -- | Returns whether or not the given position is occupied on the board. @true@ if occupied.
  isOccupied ::
    b
    -> Position
    -> Bool
  isOccupied b p =
    isJust $ playerAt b p

  -- | Returns whether or not the given position is occupied on the board. @false@ if occupied.
  isNotOccupied ::
    b
    -> Position
    -> Bool
  isNotOccupied b p =
    not (isOccupied b p)

  -- | Prints the board to standard output using an ASCII grid representation.
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

  moves (Board _ m) =
    M.size m

  isSubboardOf (Board _ m) (Board _ m') =
    m `M.isSubmapOf` m'

  isProperSubboardOf (Board _ m) (Board _ m') =
    m `M.isProperSubmapOf` m'

  playerAt (Board _ m) p =
    p `M.lookup` m

  printBoard (Board _ m) =
    printEachPosition (pos m " ")

instance BoardLike FinishedBoard where
  moveBack (FinishedBoard (Board [] _) _) =
    Nothing
  moveBack (FinishedBoard (Board ((p, _):t) m) _) =
    Just (Board t (p `M.delete` m))

  isEmpty (FinishedBoard b _) =
    isEmpty b

  occupiedPositions (FinishedBoard b _) =
    occupiedPositions b

  moves (FinishedBoard b _) =
    moves b

  isSubboardOf (FinishedBoard b _) (FinishedBoard b' _) =
    b `isSubboardOf` b'

  isProperSubboardOf (FinishedBoard b _) (FinishedBoard b' _) =
    b `isProperSubboardOf` b'

  playerAt (FinishedBoard b _) p =
    b `playerAt` p

  printBoard (FinishedBoard b _) =
    printBoard b

-- not exported

pos ::
  Ord k =>
  M.Map k Player
  -> String
  -> k
  -> String
pos m empty p =
  maybe empty (return . toSymbol) (p `M.lookup` m)

showPositionMap ::
  M.Map Position Player
  -> String
showPositionMap m =
  let pos' = pos m "?"
  in concat [ ".=",  pos' NW, "=.=", pos' N , "=.=", pos' NE
            , "=.=", pos' W , "=.=", pos' C , "=.=", pos' E
            , "=.=", pos' SW, "=.=", pos' S , "=.=", pos' SE, "=."
            ]
