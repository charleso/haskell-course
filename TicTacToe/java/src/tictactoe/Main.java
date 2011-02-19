package tictactoe;

import fj.*;
import fj.data.Option;

import static fj.Unit.unit;
import static java.lang.System.out;

public final class Main {
  private Main() {}

  private static void surround(P1<Unit> e) {
    out.println();
    out.println();
    e._1();
    out.println();
  }

  private static <B> void printBoard(final F<B, BoardLike> inheritance, final B b, final F<Position, Character> empty) {
    surround(new P1<Unit>() {
      public Unit _1() {
        out.println(inheritance.f(b).toString(new F2<Option<Player>, Position, Character>() {
          public Character f(final Option<Player> pl, final Position pos) {
            return pl.option(empty.f(pos), Player.toSymbol);
          }
        }));
        return unit();
      }
    });
  }

  private static <B> void printBoardSpaces(final F<B, BoardLike> inheritance, final B b) {
    printBoard(inheritance, b, Function.<Position, Character>constant(' '));
  }

  public static void main(final String... args) {

  }
}
