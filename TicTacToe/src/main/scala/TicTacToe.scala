class Position(x:Int, y:Int)

// class Move(pos: Position, player: Player)

class Board //(moves:Array[Move])

class EmptyBoard extends InProgressBoard

class InProgressBoard extends Board

class FinishedBoard extends Board

sealed trait Player {

  class Nought extends Player
  class Cross extends Player
}

object TicTacToe {

  /**
   * takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board.
   * This function can only be called on a board that is in-play.
   * Calling move on a game board that is finished is a *compile-time type error*.
   */
  def move(p:Position)(b:InProgressBoard):Board = b

  /**
   * takes a tic-tac-toe board and returns the player that won the game (or a draw if neither).
   * This function can only be called on a board that is finished.
   * Calling move on a game board that is in-play is a *compile-time type error*.
   */
  def whoWon(b:FinishedBoard):Player = null

  /**
   * takeBack: takes either a finished board or a board in-play that has had at least one move and returns a board in-play.
   * It is a compile-time type error to call this function on an empty board.
   */
  def takeBack(p:Position)(board:Board):Board = null

  /**
   * takes a tic-tac-toe board and position and returns the (possible) player at a given position.
   * This function works on any type of board.
   */
  def playerAt(p:Position)(b:Board):Player = null

  def positionIsOccupied(p:Position)(b:Board):Boolean = false
}
