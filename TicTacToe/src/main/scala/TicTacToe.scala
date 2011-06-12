
class Position(val x:Int, val y:Int) extends Tuple2(x, y)

class A {
  type Moves = Seq[(Position, Player)]
}

object Moves {
  def apply(): A#Moves = Nil
}

sealed abstract class Board(val moves: A#Moves) {

  /**
   * takes a tic-tac-toe board and position and returns the (possible) player at a given position.
   * This function works on any type of board.
   */
  def playerAt(p:Position):Option[Player] = moves.find(_._1 == p).flatMap((x:Tuple2[Position, Player]) => Some(x._2))

  def positionIsOccupied(p:Position):Boolean = playerAt(p).isDefined

}

class EmptyBoard extends InProgressBoard(Moves())


class InProgressBoard(override val moves: A#Moves) extends Board(moves) with TakeBack {

  /**
   * takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board.
   * This function can only be called on a board that is in-play.
   * Calling move on a game board that is finished is a *compile-time type error*.
   */
  def move(p:Position)(implicit pl: Player):Board = {
    //getPlayer(b, new Position(1, 1))
    this
  }

}

class FinishedBoard(override val moves: A#Moves) extends Board(moves) with TakeBack {

  /**
   * takes a tic-tac-toe board and returns the player that won the game (or a draw if neither).
   * This function can only be called on a board that is finished.
   * Calling move on a game board that is in-play is a *compile-time type error*.
   */
  def whoWon():Player = null
    // (0, 0), (1, 0), (2, 0)
    // (0, 1), (1, 1), (2, 1)
    // (0, 2), (1, 2), (2, 2)


}

trait TakeBack extends Board {

  /**
   * takeBack: takes either a finished board or a board in-play that has had at least one move and returns a board in-play.
   * It is a compile-time type error to call this function on an empty board.
   */
  def takeBack(p:Position):InProgressBoard = {
    new InProgressBoard(null)
  }

}

sealed trait Player
class Nought extends Player
class Cross extends Player


object TicTacToe {

  class Blah[A <: Player, B <: Player]
  implicit val n = new Blah[Nought, Cross]
  implicit val c = new Blah[Cross, Nought]

}
