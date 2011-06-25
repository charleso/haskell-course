import scala.Either.RightProjection

class Position(val x:Pos, val y:Pos) extends Tuple2(x.i, y.i)
object Position {
  implicit def t2p(t:(Pos, Pos)) = new Position(t._1, t._2)
}

class Turn(val position: Position, val player: Player) extends Tuple2(position, player)

object Moves {
  type Moves = List[Turn]
  def apply(): Moves = Nil
}
import Moves._

sealed abstract class Board(val moves: Moves) {

  /**
   * takes a tic-tac-toe board and position and returns the (possible) player at a given position.
   * This function works on any type of board.
   */
  def playerAt(p:Position):Option[Player] = moves.find(_._1 == p).map(_._2)

  def positionIsOccupied(p:Position):Boolean = playerAt(p).isDefined

}

class EmptyBoard extends Board(Moves()) with Movable

case class InProgressBoard(override val moves: Moves) extends Board(moves) with TakeBack with Movable

trait Movable extends Board {

  /**
   * takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board.
   * This function can only be called on a board that is in-play.
   * Calling move on a game board that is finished is a *compile-time type error*.
   */
  def move(p: Position)(implicit pl: Player): Either[FinishedBoard, InProgressBoard] = {
    val newMoves = new Turn(p, pl) :: moves
    def gameOver() = {
      val ourMoves = newMoves.filter(_._2 == pl).map(_._1)
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
    else if (newMoves.length == 9) Left(new DrawnBoard(newMoves))
    else Right(new InProgressBoard(newMoves))
  }

  def moveR(p: Position)(implicit pl: Player): RightProjection[FinishedBoard, InProgressBoard] = move(p).right

}

sealed trait FinishedBoard extends TakeBack {

  /**
   * takes a tic-tac-toe board and returns the player that won the game (or a draw if neither).
   * This function can only be called on a board that is finished.
   * Calling move on a game board that is in-play is a *compile-time type error*.
   */
  def whoWon: Option[Player]
}

case class WonBoard(override val moves: Moves) extends Board(moves) with FinishedBoard {

  def whoWon: Option[Player] = moves.headOption.map(_._2)
}

case class DrawnBoard(override val moves: Moves) extends Board(moves) with FinishedBoard {

  override def whoWon = None
}

trait TakeBack extends Board {

  /**
   * takeBack: takes either a finished board or a board in-play that has had at least one move and returns a board in-play.
   * It is a compile-time type error to call this function on an empty board.
   */
  def takeBack(p:Position):Either[EmptyBoard, InProgressBoard] = {
    val newMoves = moves.tail
    if (newMoves.isEmpty) Left(new EmptyBoard)
    else Right(new InProgressBoard(newMoves))
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
case object Nought extends Player("O")
case object Cross extends Player("X")

object TicTacToe {

  def main(args: Array[String]) {
    val p = new EmptyBoard()
       .moveR(($0, $0))(Nought).flatMap {
      _.moveR(($1, $1))(Cross).flatMap {
      _.move(($1, $1))(Cross)
    }}
    
    println(p)

    val x = for {
      a <- new EmptyBoard().moveR(($0, $0))(Nought)
      b <- a.moveR(($1,$1))(Cross)
      c <- b.moveR(($1,$1))(Nought)
      d <- c.moveR(($1,$1))(Cross)
      e <- d.moveR(($2,$2))(Nought)
      f <- e.moveR(($2,$2))(Cross)
      g <- f.moveR(($2,$2))(Cross)
      h <- g.moveR(($2,$2))(Cross)
      i <- h.moveR(($2,$2))(Cross)
    } yield i
    println(x)
    x.left.foreach((b:FinishedBoard) => println(b.whoWon))
  }
}
