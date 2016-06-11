module Board(
    Position(..),
    PieceColor(..),
    Board,
    boardToString,
    getInitialBoard,
    setBoardField,
    getBoardField
) where

import Data.List

data Position = Position (Int, Int)
    deriving(Read, Eq, Show)

data PieceColor = White | Black | None | Blocked
    deriving(Read, Eq, Show)

data Board = Board [[PieceColor]]
    deriving(Read, Eq, Show)

getInitialBoard :: Board
getInitialBoard = Board
    [
        [Blocked, Black, Blocked, Black, Blocked, Black, Blocked, Black],
        [Black, Blocked, Black, Blocked, Black, Blocked, Black, Blocked],
        [Blocked, Black, Blocked, Black, Blocked, Black, Blocked, Black],
        [None, Blocked, None, Blocked, None, Blocked, None, Blocked],
        [Blocked, None, Blocked, None, Blocked, None, Blocked, None],
        [White, Blocked, White, Blocked, White, Blocked, White, Blocked],
        [Blocked, White, Blocked, White, Blocked, White, Blocked, White],
        [White, Blocked, White, Blocked, White, Blocked, White, Blocked]
    ]

pieceColorToChar :: PieceColor -> Char
pieceColorToChar White = 'w'
pieceColorToChar Black = 'b'
pieceColorToChar None = '.'
pieceColorToChar Blocked = '.'

boardToString :: Board -> String
boardToString (Board list) =
    take (9 * 8 - 1) $ foldr (\row acc -> boardRowToString row ++ '\n' : acc) "" list

boardRowToString :: [PieceColor] -> String
boardRowToString =
    foldr (\field acc -> pieceColorToChar field : acc) ""

setBoardField :: Position -> PieceColor -> Board -> Board
setBoardField (Position (x, y)) color (Board listOfFields) =
    Board (replaceInListOfLists (x - 1) (y - 1) color listOfFields)

getBoardField :: Position -> Board -> PieceColor
getBoardField (Position (x, y)) (Board listOfFields) =
    (listOfFields !! (x - 1)) !! (y - 1)

replaceInListOfLists :: Int -> Int -> a -> [[a]] -> [[a]]
replaceInListOfLists x y item ls
    = replaceAtIndex x (replaceAtIndex y item (ls !! x)) ls

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b)
    where (a, (_:b)) = splitAt n ls
