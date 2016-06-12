module CheckersManager(
    MoveBox(..),
    parseMove,
    doMove
) where

import Data.List
import Data.List.Split
import qualified Board

data Move = SimpleMove (Board.Position, Board.Position)
            | KillMove [Board.Position]
    deriving(Show)

data MoveBox = MoveBox  { board :: Board.Board
                        , success :: Bool
                        , info :: String
                        } deriving (Show)

data ValidationBox = ValidationBox  { vbSuccess :: Bool
                                    , msg :: String
                                    } deriving (Show)
-- Better solution (probably) :
-- data Validation = Failed | Succeeded String
--     deriving (Show)



-- instance Monad ValidationBox where
--     return s = ValidationBox { success = s, msg = ""}
--     ValidationBox { success = False, msg = m} >>= f = ValidationBox { success = False, msg = m}

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

doMove :: Move -> Board.PieceColor -> Board.Board -> MoveBox
doMove move@(SimpleMove _) = doSimpleMove move
-- doMove move@(KillMove _) = doKillMove move

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

doSimpleMove :: Move -> Board.PieceColor -> Board.Board -> MoveBox
doSimpleMove move color board
    | color == Board.Black || color == Board.White
        = doSimpleMoveByPawn move color board
    -- | color == Board.BlackQueen || color == Board.WhiteQueen
    --     = doSimpleMoveByQueen move color board

doSimpleMoveByPawn :: Move -> Board.PieceColor -> Board.Board -> MoveBox
doSimpleMoveByPawn move@(SimpleMove (beginningPosition, endPosition)) color board =
    let boardWithUnsetPrevious =
            Board.setBoardField beginningPosition Board.None board
        ValidationBox {vbSuccess = s, msg = m} = validatePawnSimpleMove move color board
    in  if s
        then MoveBox {
            board
                = Board.setBoardField
                    endPosition
                    (managePieceColor color endPosition)
                    boardWithUnsetPrevious,
            success = True, info = ""}
        else MoveBox { board = board,
            success = False, info = "Move not Valid." ++ m}


validatePawnSimpleMove :: Move -> Board.PieceColor -> Board.Board -> ValidationBox
validatePawnSimpleMove
    (SimpleMove (
            fstPosition@(Board.Position (fstX, fstY)),
            sndPosition@(Board.Position (sndX, sndY))
        )
    )
    color board
    | not $ pieceMatchThisOnBoard color fstPosition board
        = ValidationBox {vbSuccess = False, msg = "No given color in start position."}
    | not (checkIfFieldExists fstPosition board
        && checkIfFieldExists sndPosition board)
        = ValidationBox {vbSuccess = False, msg = "No such field on board."}
    | not (checkIfFieldEmpty sndPosition board)
        = ValidationBox {vbSuccess = False, msg = "Field not empty."}
    | not (abs (fstX - sndX) == 1 && abs (fstY - sndY) == 1)
        = ValidationBox {vbSuccess = False, msg = "Field is too far"}
    |       not ((color == Board.Black || color == Board.BlackQueen) && not (sndX < fstX))
        &&  not ((color == Board.White || color == Board.WhiteQueen) && not (sndX > fstX))
        = ValidationBox {vbSuccess = False, msg = "No moving back."}
    | otherwise = ValidationBox {vbSuccess = True, msg = ""}

-- validateQueenSimpleMove

-- validatePawnKillMove

-- validateQueenKillMove

pieceMatchThisOnBoard :: Board.PieceColor -> Board.Position -> Board.Board -> Bool
pieceMatchThisOnBoard  color position board =
    let originalColor = Board.getBoardField position board
    in  Just color == originalColor

checkIfColorsAreTheSame :: Board.PieceColor -> Board.PieceColor -> Bool
checkIfColorsAreTheSame fstColor sndColor =
    let fstIsWhite = fstColor `elem` [Board.White, Board.WhiteQueen]
        sndIsWhite = sndColor `elem` [Board.White, Board.WhiteQueen]
    in fstIsWhite == sndIsWhite

checkIfFieldExists :: Board.Position -> Board.Board -> Bool
checkIfFieldExists position board =
    let boardField = Board.getBoardField position board
    in  not $ isNothing boardField

checkIfFieldEmpty :: Board.Position -> Board.Board -> Bool
checkIfFieldEmpty position board =
    let boardField = Board.getBoardField position board
    in  boardField == Just Board.None

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

validatePositionForPiece :: Board.Position -> Board.Board -> ValidationBox
validatePositionForPiece position board =
    if checkIfFieldEmpty position board
    then ValidationBox { vbSuccess = True, msg = ""}
    else ValidationBox { vbSuccess = False, msg = "You cannot put here piece."}

pieceColorCanBeMoved :: Board.PieceColor -> ValidationBox
pieceColorCanBeMoved color
    | color `elem` [Board.White, Board.WhiteQueen,
                    Board.Black, Board.WhiteQueen]
        = ValidationBox { vbSuccess = True, msg = ""}
    | otherwise = ValidationBox { vbSuccess = False, msg = "This piece cannot move."}

managePieceColor :: Board.PieceColor -> Board.Position -> Board.PieceColor
managePieceColor Board.WhiteQueen _ = Board.WhiteQueen
managePieceColor Board.BlackQueen _ = Board.BlackQueen
managePieceColor Board.White position =
    if positionOnBlacksFirstRow position
    then Board.WhiteQueen
    else Board.White
managePieceColor Board.Black position =
    if positionOnWhitesFirstRow position
    then Board.BlackQueen
    else Board.Black

positionOnBlacksFirstRow :: Board.Position -> Bool
positionOnBlacksFirstRow (Board.Position (1, _)) = True
positionOnBlacksFirstRow _ = False

positionOnWhitesFirstRow :: Board.Position -> Bool
positionOnWhitesFirstRow (Board.Position (8, _)) = True
positionOnWhitesFirstRow _ = False
