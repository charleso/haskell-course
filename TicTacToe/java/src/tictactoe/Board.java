package tictactoe;

import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.TreeMap;

import static fj.P.p;
import static fj.data.List.list;
import static tictactoe.Player.Player1;

public final class Board {
  private final List<P2<Position, Player>> moves;
  private final TreeMap<Position, Player> m;

  private static final Ord<Position> positionOrder = Ord.comparableOrd();

  private Board(final List<P2<Position, Player>> moves, final TreeMap<Position, Player> m) {
    this.moves = moves;
    this.m = m;
  }

  public static final class EmptyBoard {
    private EmptyBoard() {}

    public Board moveTo(final Position p) {
      return new Board(list(p(p, Player1)), TreeMap.<Position, Player>empty(positionOrder).set(p, Player1));
    }

    private static final EmptyBoard e = new EmptyBoard();
    public static EmptyBoard empty() {
      return e;
    }
  }
}
