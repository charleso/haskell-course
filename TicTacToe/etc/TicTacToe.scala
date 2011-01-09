sealed trait Position
case object N extends Position
case object NE extends Position
case object E extends Position
case object SE extends Position
case object S extends Position
case object SW extends Position
case object W extends Position
case object NW extends Position
case object C extends Position

object Position {
  def positions = Set(N, NE, E, SE, S, SW, W, NW, C)
}

sealed trait Player {
  def isPlayer1 = this == Player1
  def isPlayer2 = !isPlayer1

  def switch = if(isPlayer1) Player2 else Player1
}

case object Player1 extends Player
case object Player2 extends Player

sealed trait Board {
  private def moves = this match {
    case MapBoard(x, _) => x
  }

  private def map = this match {
    case MapBoard(_, x) => x
  }

  def whoseTurn = moves match {
    case Nil => Player1
    case (_, p) :: _ => p.switch
  }

  def whoseNotTurn = whoseTurn.switch

  def -->(p: Position): MoveResult = {
    val j = map get p
    val mm = map + ((p, whoseTurn))
    //val bb = Board(
    val wins = List(
                     (NW, W , SW)
                   , (N , C , S )
                   , (NE, E , SE)
                   , (NW, N , NE)
                   , (W , C , E )
                   , (SW, S , SE)
                   , (NW, C , SE)
                   , (SW, C , NE)
                   )
    def allEq[A](x: List[A]): Boolean = x match {
      case d :: e :: t => d == e && allEq(t)
      case _ => true
    }

    // Dammit scala and your missing libraries.
    // This is not Java where inadequacy is the norm. Stop copying the losers!
    def mapMOption[A, B](f: A => Option[B], as: List[A]): Option[List[B]] =
      as.map(f).foldRight[Option[List[B]]](Some(Nil))((o, z) =>
        for(oo <- o;
            zz <- z)
        yield oo :: zz)

    val isWin = wins exists {
      case (a, b, c) => mapMOption((p: Position) => map get p, List(a, b, c)) exists (allEq(_))
    }

    val isDraw = Position.positions forall (map contains _)

    error("todo")
  }

/*
- | Make a move at the given position on the given board.
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

*/
}
private final case class MapBoard(moves: List[(Position, Player)], m: collection.immutable.Map[Position, Player]) extends Board

object Board {
  private def board(moves: List[(Position, Player)], m: collection.immutable.Map[Position, Player]): Board =
    MapBoard(moves, m)

  def empty = board(Nil, Map.empty)
}

sealed trait FinishedBoard {
  private def board =
    this match {
      case FinishedBoardB(b) => b
    }
}

private final case class FinishedBoardB(b: Board) extends FinishedBoard

sealed trait MoveResult {
  def fold[X](positionAlreadyOccupied: => X,
              keepPlaying: Board => X,
              gameOver: FinishedBoard => X): X =
    this match {
      case PositionAlreadyOccupied => positionAlreadyOccupied
      case KeepPlaying(b) => keepPlaying(b)
      case GameOver(b) => gameOver(b)
    }
}
private case object PositionAlreadyOccupied extends MoveResult
private case class KeepPlaying(b: Board) extends MoveResult
private case class GameOver(b: FinishedBoard) extends MoveResult

object MoveResult {
  def positionAlreadyOccupied: MoveResult = PositionAlreadyOccupied
  def keepPlaying(b: Board): MoveResult = KeepPlaying(b)
  def gameOver(b: FinishedBoard): MoveResult = GameOver(b)
}
