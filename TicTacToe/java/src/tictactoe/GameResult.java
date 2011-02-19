package tictactoe;

import fj.F;
import fj.data.Option;

import static fj.data.Option.some;
import static tictactoe.Player.Player1;
import static tictactoe.Player.Player2;

public enum GameResult {
  Player1Wins, Player2Wins, Draw;

  public boolean isWin() {
    return this == Player1Wins || this == Player2Wins;
  }

  public boolean isDraw() {
    return !isWin();
  }

  public Option<Player> winner() {
    return this == Player1Wins ?
             some(Player1) :
             this == Player2Wins ?
               some(Player2) :
               Option.<Player>none();
  }

  @Override
  public String toString() {
    return winner().option("draw", new F<Player, String>() {
      @Override
      public String f(final Player p) {
        return p.toString() + " wins";
      }
    });
  }

  public static GameResult win(final Player p) {
    return p == Player1 ? Player1Wins : Player2Wins;
  }
}
