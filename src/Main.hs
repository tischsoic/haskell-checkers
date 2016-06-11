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
    putStrLn someText
    let board1 = Board.getInitialBoard
        board2 =
            Board.setBoardField (Board.Position (1, 5)) Board.White board1
    putStrLn $ Board.boardToString board2
    print $ Board.getBoardField (Board.Position (1, 5)) board2
