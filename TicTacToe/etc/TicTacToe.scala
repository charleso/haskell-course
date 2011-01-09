sealed trait Position
case object N extends Position
case object NE extends Position
case object E extends Position
case object SE extends Position
case object S extends Position
case object SW extends Position
case object W extends Position
case object NW extends Position
case object C extends Position

sealed trait Player {
  def isPlayer1 = this == Player1
  def isPlayer2 = this == Player2
}

case object Player1 extends Player
case object Player2 extends Player

sealed trait Board 
private final case class MapBoard(m: Map[Position, Player])

