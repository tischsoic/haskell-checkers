module Board(
    Position(..),
    Board,
    PieceColor,
    toString,
    getInitialBoard
) where

import Data.List

data Position = Position (Int, Int)
    deriving(Read, Eq, Show)

data Board = Board [Position] [Position]
    deriving(Read, Eq, Show)

data PieceColor = White | Black | None

getInitialBoard :: Board
getInitialBoard = Board
    [Position (1, 1), Position (3, 1), Position (5, 1), Position (7, 1),
    Position (2, 2), Position (4, 2), Position (6, 2), Position (8, 2),
    Position (1, 3), Position (3, 3), Position (5, 3), Position (7, 3)]

    [Position (1, 6), Position (3, 6), Position (5, 6), Position (7, 6),
    Position (2, 7), Position (4, 7), Position (6, 7), Position (8, 7),
    Position (1, 8), Position (3, 8), Position (5, 8), Position (7, 8)]

getEmptyBoardString :: [String]
getEmptyBoardString = replicate 8 $ replicate 8 '.'

toString :: Board -> String
toString board =
    let boardAsStringList = boardTotSringList board
    in intercalate "\n" boardAsStringList

boardTotSringList :: Board -> [String]
boardTotSringList (Board whitePositions blackPositions) =
    let emptyBoardString = getEmptyBoardString
        boardWithWhiteOnIt
            = foldl (\acc (Position (x, y))
                        -> replaceAtIndex (y - 1) (replaceAtIndex (x - 1) 'w' (acc !! (y - 1))) acc)
                emptyBoardString whitePositions
        boardWithWhiteAndBlackOnIt
            = foldl (\acc (Position (x, y))
                        -> replaceAtIndex (y - 1) (replaceAtIndex (x - 1) 'b' (acc !! (y - 1))) acc)
                boardWithWhiteOnIt blackPositions
    in boardWithWhiteAndBlackOnIt


replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b)
    where (a, (_:b)) = splitAt n ls
