module CheckersManager(
    parseMove,
    doMove
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
    in  if odd x
        then Board.Position (x, yBase * 2 + 2)
        else Board.Position (x, yBase * 2 + 1)

doMove :: Move -> Board.PieceColor -> Board.Board -> Board.Board
doMove move@(SimpleMove _) = doSimpleMove move
doMove move@(KillMove _) = doKillMove move

doKillMove :: Move -> Board.PieceColor -> Board.Board -> Board.Board
doKillMove (KillMove killMovesList) color board =
    let killMovesZipped = zip killMovesList $ drop 1 killMovesList
    in foldl (\board (fstPos, sndPos)
                -> doSingleKillMove fstPos sndPos color board)
        board killMovesZipped

doSingleKillMove :: Board.Position -> Board.Position
    -> Board.PieceColor -> Board.Board -> Board.Board
doSingleKillMove fstPos sndPos color board =
    let positionBetween = getFieldBetweenTwoPosition fstPos sndPos
        boardWithFstRemoved
            = Board.setBoardField fstPos Board.None board
        boardWithFstAndBetweenRemoved
            = Board.setBoardField positionBetween Board.None boardWithFstRemoved
        finalBoard
            = Board.setBoardField sndPos color boardWithFstAndBetweenRemoved
    in finalBoard

getFieldBetweenTwoPosition :: Board.Position -> Board.Position -> Board.Position
getFieldBetweenTwoPosition
    (Board.Position (fstPosX, fstPosY)) (Board.Position (sndPosX, sndPosY)) =
    let x = (fstPosX + sndPosX) `div` 2
        y = (fstPosY + sndPosY) `div` 2
    in Board.Position (x, y)

doSimpleMove :: Move -> Board.PieceColor -> Board.Board -> Board.Board
doSimpleMove (SimpleMove (beginningPosition, endPosition)) color board =
    let boardWithUnsetPrevious =
            Board.setBoardField beginningPosition Board.None board
    in Board.setBoardField endPosition color boardWithUnsetPrevious
