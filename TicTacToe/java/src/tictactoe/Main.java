package tictactoe;

import fj.*;
import fj.data.List;
import fj.data.Option;

import java.io.Console;

import static fj.Unit.unit;
import static fj.data.Option.some;
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

  private static Option<Character> readChar() {
    final String line = System.console().readLine();
    return line.isEmpty() ? Option.<Character>none() : some(line.charAt(0));
  }

  private static <B> void gameLoop(final F<B, BoardLike> inheritance, final F2<Position, B, Unit> move, final B b) {
    final Player p = inheritance.f(b).whoseTurn();
    out.println(p + " to move [" + p.toSymbol() + "]");
    out.println("  [1-9] to Move");
    out.println("  q to Quit");
    out.println("  v to view board positions");
    out.println("  > ");

    readChar().option(new P1<Unit>() {
      public Unit _1() {
        out.println("Please make a selection.");
        gameLoop(inheritance, move, b);
        return unit();
      }
    }, new F<Character, Unit>() {
      public Unit f(final Character c) {
        return Position.fromChar(c).option(new P1<Unit>() {
          public Unit _1() {
            if(c == 'q' || c == 'Q')
              out.println("Bye!");
            else {
              out.println("Invalid selection. Please try again.");
              gameLoop(inheritance, move, b);
            }
            return unit();
          }
        }, new F<Position, Unit>() {
          public Unit f(final Position d) {
            return move.f(d, b);
          }
        });
      }
    });

  }

  public static void main(final String... args) {

  }
}
