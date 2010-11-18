module Data.TicTacToe.Player
(
Player
, isPlayer1
, isPlayer2
, player1
, player2
, player
, alternate
, toSymbol
) where

data Player =
  Player1
  | Player2
  deriving (Eq, Ord, Enum)

instance Show Player where
  show Player1 = "Player 1"
  show Player2 = "Player 2"

isPlayer1 ::
  Player ->
  Bool
isPlayer1 Player1 =
  True
isPlayer1 Player2 =
  False

isPlayer2 ::
  Player
  -> Bool
isPlayer2 =
  not . isPlayer1

player1 ::
  Player
player1 =
  Player1

player2 ::
  Player
player2 =
  Player2

player ::
  x
  -> x
  -> Player
  -> x
player x _ Player1 =
  x
player _ x Player2 =
  x

alternate ::
  Player
  -> Player
alternate Player1 =
  Player2
alternate Player2 =
  Player1

toSymbol ::
  Player
  -> Char
toSymbol =
  player 'X' 'O'
