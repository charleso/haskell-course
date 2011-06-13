class Position(val x:Int, val y:Int) extends Tuple2(x, y)
object Position {
  implicit def t2p(t:(Int, Int)) = new Position(t._1, t._2)
}

// TODO Can I just have 'Moves' without the class?
class A {
  type Moves = List[(Position, Player)]
}

object Moves {
  def apply(): A#Moves = Nil
}

sealed abstract class Board(val moves: A#Moves) {

  /**
   * takes a tic-tac-toe board and position and returns the (possible) player at a given position.
   * This function works on any type of board.
   */
  // TODO Why do I need types here?!?
  def playerAt(p:Position):Option[Player] = moves.find(_._1 == p).flatMap((x:(_, Player)) => Some(x._2))

  def positionIsOccupied(p:Position):Boolean = playerAt(p).isDefined

}

class EmptyBoard extends InProgressBoard(Moves())


case class InProgressBoard(override val moves: A#Moves) extends Board(moves) with TakeBack {

  /**
   * takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board.
   * This function can only be called on a board that is in-play.
   * Calling move on a game board that is finished is a *compile-time type error*.
   */
  def move(p: Position)(implicit pl: Player): Either[FinishedBoard, InProgressBoard] = {
    val newMoves = (p, pl) :: moves
    def gameOver() = {
      val ourMoves = newMoves.filter(_._2 == pl).map(_._1)
      // TODO Generates all permutations
      val perms = for {
        a <- ourMoves
        b <- ourMoves
        c <- ourMoves
        if (a != b && b != c)
      } yield {
        def same(p:(Position => Int)) = p(a) == p(b) && p(a) == p(c)
        same(_.x) || same(_.y) ||
        (a.x == a.x && b.x == b.x && c.x == c.x)
      }
      perms.contains(true)
    }
    if (gameOver()) Left(new FinishedBoard(newMoves)) else Right(new InProgressBoard(newMoves))
  }

}

case class FinishedBoard(override val moves: A#Moves) extends Board(moves) with TakeBack {

  /**
   * takes a tic-tac-toe board and returns the player that won the game (or a draw if neither).
   * This function can only be called on a board that is finished.
   * Calling move on a game board that is in-play is a *compile-time type error*.
   */
  def whoWon():Player = moves.head._2

}

trait TakeBack extends Board {

  /**
   * takeBack: takes either a finished board or a board in-play that has had at least one move and returns a board in-play.
   * It is a compile-time type error to call this function on an empty board.
   */
  def takeBack(p:Position):InProgressBoard = {
    new InProgressBoard(moves.tail)
  }

}

// TODO Is Enumeration better?
sealed trait Player
case object Nought extends Player
case object Cross extends Player

object TicTacToe {

  def main(args: Array[String]) {
    val x = for {
      a <- new EmptyBoard().move((0, 0))(Nought).right
      b <- a.move((1,1))(Cross).right
      c <- b.move((1,1))(Nought).right
      d <- c.move((1,1))(Cross).right
      e <- d.move((2,2))(Nought).right
    } yield e
    println(x)
    x.left.foreach((b:FinishedBoard) => println(b.whoWon()))
  }
}
