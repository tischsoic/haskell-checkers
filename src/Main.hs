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
    putStrLn $ Board.toString Board.getInitialBoard
