package tictactoe;

public enum Player {
  Player1, Player2;

  public Player alternate() {
    return this == Player1 ? Player2 : Player1;
  }

  public char toSymbol() {
    return this == Player1 ? 'X' : 'O';
  }

  @Override
  public String toString() {
    return this == Player1 ? "tictactoe.Player 1" : "tictactoe.Player 2";
  }
}
