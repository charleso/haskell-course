import scala.Either.RightProjection

class Position(val x:Pos, val y:Pos)
object Position {
  implicit def t2p(t:(Pos, Pos)) = new Position(t._1, t._2)
}

class Turn[P <: Player](val position: Position, val player: P)

object Moves {
  sealed trait Moves[T <: Player] {
    val head: Option[Turn[T]]
    def tail[P <: Player](implicit m: Move[P, T]): Option[Moves[P]]
    def toList:List[Turn[_]]
    def ::[N <: Player](h: Turn[N])(implicit m: Move[T, N]) = {
      val that = this
      new Moves[N] {
        val head = Some(h)
        def tail[P <: Player](implicit m: Move[P, N]) = Some(that.asInstanceOf[Moves[P]])
        def toList = h :: that.toList
      }
    }
  }
  case class NilMove[T <: Player]() extends Moves[T] {
    override val head = None
    override def tail[P <: Player](implicit m: Move[P, T]) = None
    override def toList = Nil
  }
  def apply[T <: Player]: Moves[T] = NilMove[T]
}
import Moves._

sealed abstract class Board[P <: Player](val moves: Moves[P]) {

  /**
   * takes a tic-tac-toe board and position and returns the (possible) player at a given position.
   * This function works on any type of board.
   */
  def playerAt(p:Position):Option[Player] = moves.toList.find(_.position == p).map(_.player.asInstanceOf[Player])

  def positionIsOccupied(p:Position):Boolean = playerAt(p).isDefined

}

class EmptyBoard[T <: Player] extends Board(Moves[T]) with Movable[T]

case class InProgressBoard[T <: Player](override val moves: Moves[T]) extends Board(moves) with TakeBack[T] with Movable[T]

trait Movable[P <: Player] extends Board[P] {

  /**
   * takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board.
   * This function can only be called on a board that is in-play.
   * Calling move on a game board that is finished is a *compile-time type error*.
   */
  def move[N <: Player](p: Position)(implicit m: Move[P, N]): Either[FinishedBoard[N], InProgressBoard[N]] = {
    val pl = m.next
    val newMoves = new Turn(p, pl) :: moves
    def gameOver() = {
      val ourMoves = newMoves.toList.filter(_.player == pl).map(_.position)
      // TODO Generates all permutations
      (for {
        a <- ourMoves; b <- ourMoves; c <- ourMoves
        if (a != b && a != c && b != c)
      } yield {
        def same(p:(Position => Pos)) = p(a) == p(b) && p(a) == p(c)
        same(_.x) || same(_.y) ||
        (a.x == a.y && b.x == b.y && c.x == c.y)
      }) contains true
    }
    if (gameOver()) Left(new WonBoard(newMoves))
    else if (newMoves.toList.length == 9) Left(new DrawnBoard(newMoves))
    else Right(new InProgressBoard(newMoves))
  }

  def moveR[N <: Player](p: Position)(implicit m: Move[P, N]): RightProjection[FinishedBoard[N], InProgressBoard[N]] = move(p).right

}

sealed trait FinishedBoard[T <: Player] extends TakeBack[T] {

  /**
   * takes a tic-tac-toe board and returns the player that won the game (or a draw if neither).
   * This function can only be called on a board that is finished.
   * Calling move on a game board that is in-play is a *compile-time type error*.
   */
  def whoWon: Option[T]
}

case class WonBoard[T <: Player](override val moves: Moves[T]) extends Board(moves) with FinishedBoard[T] {

  def whoWon = moves.head.map(_.player.asInstanceOf[T])
}

case class DrawnBoard[T <: Player](override val moves: Moves[T]) extends Board(moves) with FinishedBoard[T] {

  override def whoWon = None
}

trait TakeBack[N <: Player] extends Board[N] {

  /**
   * takeBack: takes either a finished board or a board in-play that has had at least one move and returns a board in-play.
   * It is a compile-time type error to call this function on an empty board.
   */
  def takeBack[T <: Player](p: Position)(implicit m: Move[T, N]): Either[EmptyBoard[T], InProgressBoard[T]] = {
    val newMoves = moves.tail
    if (newMoves.isEmpty) Left(new EmptyBoard[T])
    else Right(new InProgressBoard[T](newMoves.get))
  }

}

sealed abstract class Pos(val i:Int)
object $0 extends Pos(0);
object $1 extends Pos(1);
object $2 extends Pos(2);
object Pos {
  def apply(i: Int) = i match {
    case 0 => $0
    case 1 => $1
    case 2 => $2
    case _ => throw new Exception("Invalid pos: " + i)
  }
}

sealed abstract class Player(override val toString:String)
case class Nought() extends Player("O")
case class Cross() extends Player("X")

sealed class Move[P, N](val next:N)

object Player {
  implicit val n2c = new Move[Nought, Cross](new Cross)
  implicit val c2n = new Move[Cross, Nought](new Nought)
}
