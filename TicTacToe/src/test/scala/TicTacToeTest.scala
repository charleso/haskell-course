import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

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

class TicTacToeTest extends FlatSpec with ShouldMatchers {
  "TicTacToe" should "Win" in {
    (for {
      a <- new EmptyBoard().moveR(($0, $0))(Nought)
      b <- a.moveR(($1, $1))(Cross)
      c <- b.moveR(($1, $1))(Nought)
      d <- c.moveR(($1, $1))(Cross)
      e <- d.moveR(($2, $2))(Nought)
    } yield e).left.map(_.whoWon) should equal(Left(Some(Nought)))
  }

  "TicTacToe" should "Draw" in {
    (for {
      a <- new EmptyBoard().moveR(($0, $0))(Nought)
      b <- a.moveR(($0, $1))(Cross)
      c <- b.moveR(($0, $2))(Nought)
      d <- c.moveR(($1, $2))(Cross)
      e <- d.moveR(($2, $2))(Nought)
      f <- e.moveR(($1, $1))(Cross)
      g <- f.moveR(($1, $0))(Nought)
      h <- g.moveR(($2, $0))(Cross)
      i <- h.moveR(($2, $1))(Nought)
    } yield i).left.map(_.whoWon) should equal(Left(None))
  }

  "TicTacToe" should "In Progress" in {
    (for {
      a <- new EmptyBoard().moveR(($0, $0))(Nought)
      b <- a.moveR(($0, $1))(Cross)
    } yield b).isRight should equal(true)
  }
}
