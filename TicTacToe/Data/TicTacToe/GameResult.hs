module Data.TicTacToe.GameResult
(
  GameResult
, gameResult
, playerGameResult
, win
, player1Wins
, player2Wins
, draw
, isPlayer1Wins
, isPlayer2Wins
, isDraw
) where

import Data.TicTacToe.Player

data GameResult =
  Win Player
  | Draw
  deriving (Eq, Show)

gameResult ::
  (Player -> x)
  -> x
  -> GameResult
  -> x
gameResult win _    (Win p) =
  win p
gameResult _   draw Draw    =
  draw

playerGameResult ::
  x
  -> x
  -> x
  -> GameResult
  -> x
playerGameResult p1 p2 =
  gameResult (player p1 p2)

win ::
  Player
  -> GameResult
win =
  Win

player1Wins ::
  GameResult
player1Wins =
  Win player1

player2Wins ::
  GameResult
player2Wins =
  Win player1

draw ::
  GameResult
draw =
  Draw

isPlayer1Wins ::
  GameResult
  -> Bool
isPlayer1Wins =
  playerGameResult True False False

isPlayer2Wins ::
  GameResult
  -> Bool
isPlayer2Wins =
  playerGameResult False True False

isDraw ::
  GameResult
  -> Bool
isDraw =
  playerGameResult False False True

