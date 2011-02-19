package tictactoe;

import fj.*;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

import static fj.P.p;
import static fj.data.List.list;
import static fj.data.List.nil;
import static fj.data.Option.none;
import static tictactoe.GameResult.Draw;
import static tictactoe.Player.Player1;
import static tictactoe.Position.*;

public final class Board extends BoardLike {
  private final List<P2<Position, Player>> moves;
  private final TreeMap<Position, Player> m;

  private static final Ord<Position> positionOrder = Ord.comparableOrd();

  private Board(final List<P2<Position, Player>> moves, final TreeMap<Position, Player> m) {
    this.moves = moves;
    this.m = m;
  }

  public Player whoseTurn() {
    return moves.isEmpty() ? Player1 : moves.head()._2().alternate();
  }

  public boolean isEmpty() {
    return false;
  }

  public List<Position> occupiedPositions() {
    return m.keys();
  }

  public int nmoves() {
    return m.size();
  }

  public Option<Player> playerAt(Position p) {
    return m.get(p);
  }

  @SuppressWarnings("unchecked")
  public MoveResult moveTo(final Position p) {
    final Player wt = whoseTurn();
    final Option<Player> j = m.get(p);
    final TreeMap<Position, Player> mm = m.set(p, wt);
    final Board bb = new Board(moves.cons(P.p(p, wt)), mm);
    final List<P3<Position, Position, Position>> wins =
        list(
            P.p(NW, W, SW)
            , P.p(N, C, S)
            , P.p(NE, E, SE)
            , P.p(NW, N, NE)
            , P.p(W, C, E)
            , P.p(SW, S, SE)
            , P.p(NW, C, SE)
            , P.p(SW, C, NE)
        );
    final boolean isWin = wins.exists(new F<P3<Position, Position, Position>, Boolean>() {
      public Boolean f(final P3<Position, Position, Position> abc) {
        return list(abc._1(), abc._2(), abc._3()).mapMOption(m.get()).exists(new F<List<Player>, Boolean>() {
          public Boolean f(final List<Player> ps) {
            return ps.allEqual(Equal.<Player>anyEqual());
          }
        });
      }
    });

    final boolean isDraw = Position.positions().forall(new F<Position, Boolean>() {
      public Boolean f(final Position p) {
        return m.contains(p);
      }
    });

    return j.isSome() ?
        MoveResult.positionAlreadyOccupied() :
        isWin ?
            MoveResult.gameOver(new FinishedBoard(bb, GameResult.win(wt))) :
            isDraw ?
                MoveResult.gameOver(new FinishedBoard(bb, Draw)) :
                MoveResult.keepPlaying(bb);
  }

  public static final class EmptyBoard extends BoardLike {
    private EmptyBoard() {}

    @SuppressWarnings("unchecked")
    public Board moveTo(final Position p) {
      return new Board(list(p(p, Player1)), TreeMap.<Position, Player>empty(positionOrder).set(p, Player1));
    }

    private static final EmptyBoard e = new EmptyBoard();
    public static EmptyBoard empty() {
      return e;
    }

    public Player whoseTurn() {
      return Player1;
    }

    public boolean isEmpty() {
      return true;
    }

    public List<Position> occupiedPositions() {
      return nil();
    }

    public int nmoves() {
      return 0;
    }

    public Option<Player> playerAt(Position p) {
      return none();
    }
  }

  public static final class FinishedBoard {
    private final Board b;
    private final GameResult r;

    private FinishedBoard(final Board b, final GameResult r) {
      this.b = b;
      this.r = r;
    }

  }
}
