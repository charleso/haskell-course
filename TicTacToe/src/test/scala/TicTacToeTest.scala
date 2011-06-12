import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import TicTacToe._

class TicTacToeTest extends Properties("TicTacToe") {

  implicit def arbitraryBoard[A]: Arbitrary[InProgressBoard] =
    Arbitrary(Gen.fail[InProgressBoard])

  implicit def arbitraryPosition[A]: Arbitrary[Position] =
    Arbitrary(Gen.fail[Position])

  /**
   * forall Board b. forall Position p. such that
   * (not (positionIsOccupied p b)). takeBack(move(p, b)) == b
   */
  property("something") = Prop.forAll((b: InProgressBoard, p: Position) =>
    b.positionIsOccupied(p) || (b.move(p).takeBack(p)) != b)
}