import scala.Either.RightProjection

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

  // TODO This should be a fold over the list
  def draw = {
    var sorted = moves.sortBy((p:(Position, _)) => p._1.x + (p._1.y * 3))
    val b = new StringBuilder
    for {
      y <- 0 until 3
      x <- 0 until 3
    } {
      if (x == 0 && y != 0) {
        b.append("\n");
      }
      if (sorted.headOption.forall((p:(Position, Player)) => p._1.x == x && p._1.y == y)) {
        b.append(sorted.head._2)
        sorted = sorted.tail
      } else {
        b.append(" ")
      }
    }
    b
  }
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
      (for {
        a <- ourMoves; b <- ourMoves; c <- ourMoves
        if (a != b && a != c && b != c)
      } yield {
        def same(p:(Position => Int)) = p(a) == p(b) && p(a) == p(c)
        same(_.x) || same(_.y) ||
        (a.x == a.y && b.x == b.y && c.x == c.y)
      }) contains true
    }
    if (gameOver()) Left(new FinishedBoard(newMoves))
    if (newMoves.length == 9) Left(new DrawnBoard(newMoves))
    else Right(new InProgressBoard(newMoves))
  }

  def moveR(p: Position)(implicit pl: Player): RightProjection[FinishedBoard, InProgressBoard] = move(p).right

}

case class FinishedBoard(override val moves: A#Moves) extends Board(moves) with TakeBack {

  /**
   * takes a tic-tac-toe board and returns the player that won the game (or a draw if neither).
   * This function can only be called on a board that is finished.
   * Calling move on a game board that is in-play is a *compile-time type error*.
   */
  def whoWon:Option[Player] = Some(moves.head._2)

}

case class DrawnBoard(override val moves: A#Moves) extends FinishedBoard(moves) {

  override def whoWon = None

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
sealed abstract class Player(override val toString:String)
case object Nought extends Player("O")
case object Cross extends Player("X")

object TicTacToe {

  def main(args: Array[String]) {
    val p = new EmptyBoard()
       .moveR((0, 0))(Nought).flatMap {
      _.moveR((1, 1))(Cross).flatMap {
      _.move((1, 1))(Cross)
    }}
    
    println(p)

    val x = for {
      a <- new EmptyBoard().moveR((0, 0))(Nought)
      b <- a.moveR((1,1))(Cross)
      c <- b.moveR((1,1))(Nought)
      d <- c.moveR((1,1))(Cross)
      e <- d.moveR((2,2))(Nought)
      f <- e.moveR((2,2))(Cross)
      g <- f.moveR((2,2))(Cross)
      h <- g.moveR((2,2))(Cross)
      i <- h.moveR((2,2))(Cross)
    } yield i
    println(x)
    x.left.foreach((b:FinishedBoard) => println(b.whoWon))
  }
}
