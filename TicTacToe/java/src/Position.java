import fj.data.List;
import fj.data.Option;

import static fj.data.List.list;
import static fj.data.Option.none;
import static fj.data.Option.some;

public enum Position {
  NW, N, NE, W, C, E, SW, S, SE;

  int toInt() {
    return ordinal() + 1;
  }

  char toChar() {
    return (char)(toInt() + '0');
  }

  static List<Position> positions() {
    return list(NW, N, NE, W, C, E, SW, S, SE);
  }

  static Option<Position> fromInt(final int n) {
    switch(n) {
      case 1: return some(NW);
      case 2: return some(N );
      case 3: return some(NE);
      case 4: return some(W );
      case 5: return some(C );
      case 6: return some(E );
      case 7: return some(SW);
      case 8: return some(S);
      case 9: return some(SE);
      default: return none();
    }
  }
}
