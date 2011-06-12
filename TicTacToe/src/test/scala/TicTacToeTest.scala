import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import TicTacToe._

class TicTacToeTest extends Properties("TicTacToe") {

  implicit def arbitraryBoard[A]: Arbitrary[Board] =
    Arbitrary(Gen.fail[Board])

  implicit def arbitraryPosition[A]: Arbitrary[Position] =
    Arbitrary(Gen.fail[Position])

  /**
   * forall Board b. forall Position p. such that
   * (not (positionIsOccupied p b)). takeBack(move(p, b)) == b
   */
  property("something") = Prop.forAll((b: Board, p: Position) =>
    positionIsOccupied(p)(b) || (move(p) _ andThen takeBack(p) _)(b) != b)
}