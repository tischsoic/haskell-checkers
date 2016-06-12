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
    let colorStr = show color
    putStrLn ("Current move by: " ++ colorStr)
    moveStr <- getLine
    let move = parseMove moveStr
        MoveBox {
            board = changedBoard,
            success = s,
            info = i } = doMove move color board
    putStrLn $ Board.boardToString changedBoard
    putStrLn ("Info: " ++ i)
    if s
    then managePlay (getOppositeColor color) changedBoard
    else managePlay color changedBoard

getOppositeColor :: Board.PieceColor -> Board.PieceColor
getOppositeColor color
    | color == Board.White = Board.Black
    | color == Board.Black = Board.White
