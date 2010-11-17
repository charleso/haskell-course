module Data.TicTacToe.Position where

data Position =
  NW
  | N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | C
  deriving (Eq, Ord, Enum, Bounded)
