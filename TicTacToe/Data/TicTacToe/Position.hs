module Data.TicTacToe.Position
(
  Position(..)
) where

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

instance Show Position where
  show NW = "NW"
  show N  = "N "
  show NE = "NE"
  show E  = "E "
  show SE = "SE"
  show S  = "S "
  show SW = "SW"
  show W  = "W "
  show C  = "C "
