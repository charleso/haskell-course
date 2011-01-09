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

sealed trait Player {
  def isPlayer1 = this == Player1
  def isPlayer2 = this == Player2
}

case object Player1 extends Player
case object Player2 extends Player

sealed trait Board {
  private def withMovesMap[X](f: (List[(Position, Player)], Map[Position, Player]) => X): X =
    this match {
      case MapBoard(moves, m) => f(moves, m)
    }

  private def withMoves[X](f: List[(Position, Player)] => X): X =
    withMovesMap((moves, _) => f(moves))
}
private final case class MapBoard(moves: List[(Position, Player)], m: Map[Position, Player]) extends Board

object Board {
  def empty: Board = MapBoard(Nil, Map.empty)
}

