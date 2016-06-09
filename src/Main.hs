module Main (main) where

import System.IO
import System.Environment
import Data.List
-- import Data.Map (Map)
-- import qualified Data.Map as Map
import Data.Char
import Control.Monad

-- import Board2
-- import Test.HUnit
-- import Test.HUnit

-- import Board
import Board


main :: IO ()
main = do
    word <- getLine
    print getDumbText
    print $ DataBox 12 "as"
    print $ DataBox (Just 2) "asd"
    print word
