module Main where

import System.IO
import System.Console.ANSI
import Prelude hiding (Either(..))

main :: IO ()
main = do
  hSetEcho stdin False -- Hides input characters
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Thieflike"
  gameLoop (0,0)

type Coord = (Int, Int)

data Input
  = Up
  | Down
  | Left
  | Right
  | Exit
  deriving (Eq)

drawPlayer :: Coord -> IO ()
drawPlayer (x,y) = do
  clearScreen
  setCursorPosition y x
  putStr "@"

gameLoop :: Coord -> IO ()
gameLoop coord = do
  drawPlayer coord
  input <- getInput
  case input of
    Exit -> exit
    _    -> handleInput coord input

-- Updates the coordinates according to the input (direction) and starts a new gameLoop
handleInput :: Coord -> Input -> IO ()
handleInput coord input = do
  gameLoop $ updateCoord coord input
  where updateCoord (x,y) input = case input of
          Up    -> (x, y-1)
          Down  -> (x, y+1)
          Left  -> (x-1, y)
          Right -> (x+1, y)

-- Gets char (input) until it is w, s, d, a or q
getInput = do
  evaluateInput =<< getChar
  where evaluateInput char = case char of
          'w' -> return Up
          's' -> return Down
          'd' -> return Right
          'a' -> return Left
          'q' -> return Exit
          _   -> getInput

exit :: IO ()
exit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "thanks for playing (:"
