import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import TicTacToe._

object TicTacToeTest extends Properties("TicTacToe") {

  implicit def arbitraryPlayer[A]: Arbitrary[Player] = Arbitrary(Gen.oneOf(Cross, Nought))

  implicit def arbitraryBoard[A]: Arbitrary[Movable] =
    Arbitrary(Gen.choose(0, 7).map((x: Int) => {
      // TODO A var - I cheated! What is a better way?
      var b: Movable = new EmptyBoard()
      for {
        i <- 0 to x
        player <- arbitrary[Player].sample
        pos <- arbitrary[Position].sample
      } {
        b = b.moveR(pos)(player).getOrElse(b)
      }
      b
    }))

  implicit def arbitraryPosition[A]: Arbitrary[Position] =
    Arbitrary(Gen.choose(0, 8).map((x: Int) => (Pos(x % 3), Pos(x / 3))))

  /**
   * forall Board b. forall Position p. such that
   * (not (positionIsOccupied p b)). takeBack(move(p, b)) == b
   */
  property("moveAndTakeBack") = Prop.forAll((b: Movable, p: Position, pl: Player) =>
    b.positionIsOccupied(p) || (b.move(p)(pl).merge.takeBack(p).merge) == b
  )
}