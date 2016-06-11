module CheckersManager(
    parseMove
) where

import Data.List
import Data.List.Split
import qualified Board

data Move = SimpleMove (Board.Position, Board.Position)
            | KillMove [Board.Position]
    deriving(Read, Eq, Show)

parseMove :: String -> Move
parseMove moveString
    | '-' `elem` moveString = parseSimpleMove moveString
    | otherwise = parseKillMove moveString

parseKillMove :: String -> Move
parseKillMove moveString =
    let moves = splitOn "x" moveString
        movesInt = map read moves :: [Int]
        movesPositions = map fromStandarisedPositionToPosition movesInt
    in KillMove movesPositions

parseSimpleMove :: String -> Move
parseSimpleMove moveString =
    let moves = splitOn "-" moveString
        firstMove = head moves
        secondMove = moves !! 1
        firstMoveInt = read firstMove :: Int
        secondMoveInt = read secondMove :: Int
        firstMovePosition = fromStandarisedPositionToPosition firstMoveInt
        secondMovePosition = fromStandarisedPositionToPosition secondMoveInt
    in SimpleMove (firstMovePosition, secondMovePosition)

fromStandarisedPositionToPosition :: Int -> Board.Position
fromStandarisedPositionToPosition position =
    let positionBase = position - 1
        xBase = positionBase `div` 4
        yBase = positionBase `mod` 4
        x = xBase + 1
        xIsOdd = odd x
    in  if xIsOdd
        then Board.Position (x, yBase * 2 + 2)
        else Board.Position (x, yBase * 2 + 1)
