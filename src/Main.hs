module Main (main) where

import System.IO
import System.Environment
import Data.List
import Data.Char
import Control.Monad

import qualified Board
import CheckersManager

main :: IO ()
main = do
    let initialBoard = Board.getInitialBoard
    managePlay Board.Black initialBoard

managePlay :: Board.PieceColor -> Board.Board -> IO ()
managePlay color board = do
    moveStr <- getLine
    let move = parseMove moveStr
        changedBoard = doMove move color board
    putStrLn $ Board.boardToString changedBoard
    managePlay (getOppositeColor color) changedBoard

getOppositeColor :: Board.PieceColor -> Board.PieceColor
getOppositeColor color
    | color == Board.White = Board.Black
    | color == Board.Black = Board.White
