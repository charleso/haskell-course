import org.scalacheck._
import TicTacToe._

class TicTacToeTest extends Properties("TicTacToe") {

  /**
   * forall Board b. forall Position p. such that
   * (not (positionIsOccupied p b)). takeBack(move(p, b)) == b
   */
  property("something") = Prop.forAll((b: Board, p: Position) =>
    (!(positionIsOccupied(takeBack(move(b, p)), p))) == b)
}