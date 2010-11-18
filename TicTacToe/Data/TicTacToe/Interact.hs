module Data.TicTacToe.Interact where

import Data.TicTacToe.Board
import Data.TicTacToe.Position
import Data.TicTacToe.Player
import Data.TicTacToe.GameResult
import Data.Char

tictactoe =
  tictactoe' empty

tictactoe' b =
  let p = whoseTurn b
  in do putStrLn $ (show p) ++ " to move [" ++ [toSymbol p] ++ "]"
        putStrLn "  [1-9] to Move"
        putStrLn "  q to Quit"
        putStrLn "  v to view board positions"
        putStr "  > "
        c <- getChar
        if c `elem` "vV"
          then
            do putStrLn []
               putStrLn []
               printWithPositions b
               putStrLn []
               tictactoe' b
          else
            if c `elem` ['1'..'9']
              then
                case toPosition (digitToInt c) --> b
                of PositionAlreadyOccupied -> do putStrLn []
                                                 putStrLn []
                                                 putStrLn "That position is already taken. Try again."
                                                 putStrLn []
                                                 printWithoutPositions b
                                                 putStrLn []
                                                 tictactoe' b
                   KeepPlaying b'          -> do putStrLn []
                                                 putStrLn []
                                                 printWithoutPositions b'
                                                 putStrLn []
                                                 tictactoe' b'
                   GameFinished b'         -> do putStrLn []
                                                 putStrLn []
                                                 printWithoutPositions b'
                                                 putStrLn []
                                                 putStrLn (playerGameResult "Player 1 Wins!" "Player 2 Wins!" "Draw" (getResult b'))

              else
                if c `elem` "qQ"
                  then
                    do putStrLn []
                       putStrLn "Bye!"
                  else
                    do putStrLn []
                       putStrLn []
                       putStrLn "Invalid selection. Please try again."
                       putStrLn []
                       tictactoe' b

printWithPositions b =
  printEachPosition (\p -> maybe (show . fromPosition $ p) (return . toSymbol) (b `playerAt` p))

printWithoutPositions b =
  printEachPosition (\p -> maybe " " (return . toSymbol) (b `playerAt` p))

-- not exported

fromPosition ::
  Position
  -> Int
fromPosition =
  succ . fromEnum

toPosition ::
  Int
  -> Position
toPosition n =
  toEnum (n - 1)

