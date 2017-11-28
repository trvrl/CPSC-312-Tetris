module Engine where

import Types
import Data.List
import Data.Maybe
import Data.Matrix
import Data.Bool
import qualified Data.Vector as Vector
import Graphics.Gloss.Data.Color
import System.Random

initial = Tetris 0 (tetromino L) initEmptyBoard (mkStdGen 1)

steps :: Int
steps = 2

squareSizeT, height, heightEnd, width :: Int
width = 10
height = 24
heightEnd = 24
squareSizeT = 20

-- Takes in a Matrix column (as a Vector) and returns the occupied cell with the smallest y value and its index.
getHighestSquareInColumn :: Vector.Vector Cell -> (Cell, Int)
getHighestSquareInColumn vec =  getMinY vec 1

-- Returns the cell and index of the lowest indexed occupied cell
getMinY :: Vector.Vector Cell -> Int -> (Cell,Int)
getMinY vec n = if occ1 then ((Vector.head vec),n) else if n == 24 then (Cell{ x = x1, y = y1, col = col1, occ = occ1}, (height+1)) else getMinY (Vector.tail vec) (n+1)
    where 
        x1 = x (Vector.head vec)
        y1 = y (Vector.head vec) + 1
        col1 = col (Vector.head vec)
        occ1 = occ (Vector.head vec)

-- Given a set of columns, returns the cell, rowindex and columnindex of the lowest indexed occupied square
getHighestSquareBelowTetromino :: Int -> Int -> Matrix Cell -> (Cell,Int,Int)
getHighestSquareBelowTetromino startCol endCol gameBoard
    | startCol == endCol = (cell, index, startCol)
    | otherwise = if index <= rowindex then (cell, index, startCol) else (cell1, rowindex, colindex)
    where
        (cell, index) = getHighestSquareInColumn (getCol startCol gameBoard)
        (cell1, rowindex, colindex) = getHighestSquareBelowTetromino (startCol + 1) endCol gameBoard

-- Given a set of columns, returns all the cells, row indices and column indices of the lowest indexed occuped square
getAllHighestSquaresInRange :: Int -> Int -> Matrix Cell -> [(Cell,Int,Int)]
getAllHighestSquaresInRange startCol endCol gameBoard
    | startCol == endCol = [(cell, index, startCol)]
    | otherwise = (cell, index, startCol):(getAllHighestSquaresInRange (startCol+1) endCol gameBoard)
    where
        (cell, index) = getHighestSquareInColumn (getCol startCol gameBoard)

-- Creates an empty game board
initEmptyBoard :: Matrix Cell
initEmptyBoard = matrix height width (\(y1,x1) -> Cell{ x = x1 - 1, y = y1 - 1, col = black, occ = False})

-- Checks the game for things to be updated
checkGame :: Tetris -> Tetris
checkGame (Tetris points tetromino board randGen) = (Tetris points tetromino (clearFilledRows height board) randGen)

-- Given a board, checks to see if the game has been lost
gameLost :: Matrix Cell -> Bool
gameLost board = foldr (\ (cell,rowNum,colNum) r -> if rowNum < 5 then True || r else False || r ) False (getAllHighestSquaresInRange 1 width board) 

-- Returns the points scored
getScore :: Matrix Cell -> Int
getScore board
    | rowsCleared < 4 = 100 * rowsCleared
    | rowsCleared >= 4 = 200 * rowsCleared
    where rowsCleared = length(checkNRows height board)

-- Removes empty rows on a board
clearFilledRows :: Int -> Matrix Cell -> Matrix Cell
clearFilledRows row board
    | (row == 1) && (checkNthRow row board) = clearTopRow board
    | (row == 1) && not (checkNthRow row board) = board
    | checkNthRow row board = clearFilledRows row (iterateRow row height board initEmptyBoard)
    | otherwise  = clearFilledRows (row-1) board

-- Empties the top row of the board
clearTopRow :: Matrix Cell -> Matrix Cell
clearTopRow board = emptyRow 1 board

-- Empties a given row n on a board
emptyRow :: Int -> Matrix Cell -> Matrix Cell
emptyRow n board = mapRow (\ _ cell -> Cell{ x = (x cell), y = (y cell), col = black, occ = False}) n board

-- Helper function for clearing filled rows and copying non-filled rows
iterateRow :: Int -> Int -> Matrix Cell -> Matrix Cell -> Matrix Cell
iterateRow startInt totalInt oldBoard newBoard
    | startInt == 1 = newBoard
    | startInt <= totalInt = iterateRow startInt (totalInt-1) oldBoard (iterateRowCopy totalInt width oldBoard newBoard copyEntireRow)
    | otherwise = iterateRow (startInt-1) (totalInt-1) oldBoard (iterateRowCopy startInt width oldBoard newBoard copyElementDown)

-- Helper function which takes in a function and applies it to every element of a particular row
iterateRowCopy :: Int -> Int -> Matrix Cell -> Matrix Cell -> (Int -> Int -> Matrix Cell -> Matrix Cell -> Matrix Cell) -> Matrix Cell 
iterateRowCopy rowNum colNum oldBoard newBoard fn
    | colNum == 0 = newBoard
    | otherwise = iterateRowCopy rowNum (colNum-1) oldBoard (fn rowNum colNum oldBoard newBoard) fn

-- Copies the contents of a cell to the same location in a new board
copyEntireRow :: Int -> Int -> Matrix Cell -> Matrix Cell -> Matrix Cell
copyEntireRow rowNum colNum oldBoard newBoard = setElem newReplaceCell (rowNum,colNum) newBoard
    where
        oldCell = getElem rowNum colNum oldBoard
        occ1 = occ oldCell
        col1 = col oldCell
        newCell = getElem rowNum colNum newBoard
        x1 = x newCell
        y1 = y newCell
        newReplaceCell = Cell {x = x1, y = y1, col = col1, occ = occ1}

-- Copies the contents of a cell to a location one row below the current location
copyElementDown :: Int -> Int -> Matrix Cell -> Matrix Cell -> Matrix Cell    
copyElementDown rowNum colNum oldBoard newBoard = setElem newReplaceCell (rowNum,colNum) newBoard
    where
        oldCell = getElem (rowNum-1) colNum oldBoard
        occ1 = occ oldCell
        col1 = col oldCell
        newCell = getElem rowNum colNum newBoard
        x1 = x newCell
        y1 = y newCell
        newReplaceCell = Cell {x = x1, y = y1, col = col1, occ = occ1}

-- Checks the top given number of rows for completion  
checkNRows :: Int -> Matrix Cell -> [Int]
checkNRows n board
    | n == 0 = []
    | otherwise = if checkRow (getRow n board) then n:(checkNRows (n-1) board) else checkNRows (n-1) board

-- Checks the nth row and determines if it is filled
checkNthRow :: Int -> Matrix Cell -> Bool
checkNthRow n board = checkRow (getRow n board)
    
-- Takes in a Matrix row (as a Vector) and returns True if all cells in a row are occupied
checkRow :: Vector.Vector Cell -> Bool
checkRow vec = if null vec then True else occ1 && (checkRow (Vector.tail vec))
    where 
        occ1 = occ (Vector.head vec)

-- iterates the board to a new state every step of play
step :: Float -> Tetris -> Tetris
step _ (Tetris points tetromino board randGen) = (Tetris points tetromino board randGen)

-- moves a tetromino up in the play area
moveUp :: Tetromino -> Tetromino
moveUp (Tetromino mino rotation minoPos) = Tetromino mino rotation newPos
    where newPos = move minoPos(0, -1)

-- moves a tetromino down in the play area
moveDown :: Tetromino -> Tetromino
moveDown (Tetromino mino rotation minoPos) = Tetromino mino rotation newPos
    where newPos = move minoPos(0, 1)

-- moves a tetromino right in the play area
moveRight :: Tetromino -> Tetromino
moveRight (Tetromino mino rotation minoPos) = Tetromino mino rotation newPos
    where newPos = move minoPos (1, 0)

-- moves a tetromino left in the play area
moveLeft :: Tetromino -> Tetromino
moveLeft (Tetromino mino rotation minoPos) = Tetromino mino rotation newPos 
    where newPos = move minoPos (-1, 0)

-- rotates a tetromino right (CW) in the play area
rotateRight :: Tetromino -> Tetromino
rotateRight (Tetromino mino from position) = Tetromino mino newRot newPos where 
    to = if (from < 3) then from + 1 else 0
    (newRot, newPos) = rotate mino from to position

-- rotates a tetromino left (CCW) in the play area
rotateLeft :: Tetromino -> Tetromino
rotateLeft (Tetromino mino from position) = Tetromino mino newRot newPos where
    to = if (from > 0) then from - 1 else 3
    (newRot, newPos) = rotate mino from to position

-- creates a tetromino given the mino type in the initial position and rotation
tetromino :: Mino -> Tetromino
tetromino mino = Tetromino mino 0 (initPos mino)

-- initial position of the four squares of a specific tetromino type
initPos :: Mino -> MinoPos
initPos I = minoPos I 0 (3, 3)
initPos O = minoPos O 0 (4, 2)
initPos mino = minoPos mino 0 (4, 3)

-- applies function to all tetromino square positions
mapPos :: (Coord -> t) -> MinoPos -> (t, t, t, t)
mapPos f (a, b, c, d) = (f a, f b, f c, f d)

-- creates a list from the tetromino square positions
posToList :: MinoPos -> [Coord]
posToList (a, b, c, d) = [a, b, c, d]

-- dictates the position of the tetromino in their various rotations
-- I Positions
minoPos :: Mino -> Int -> Coord -> MinoPos

minoPos I 1 (x, y) = ( (x, y), (x, y + 1), (x, y + 2), (x, y + 3) )
minoPos I 2 (x, y) = ( (x, y), (x - 1, y), (x - 2, y), (x - 3, y) )
minoPos I 3 (x, y) = ( (x, y), (x, y - 1), (x, y - 2), (x, y - 3) )
minoPos I _ (x, y) = ( (x, y), (x + 1, y), (x + 2, y), (x + 3, y) )

-- O Positions
minoPos O _ (x, y) = ( (x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1) )

-- T Positions
minoPos T 1 (x, y) = ( (x, y), (x, y - 1), (x + 1, y), (x, y + 1) )
minoPos T 2 (x, y) = ( (x, y), (x + 1, y), (x, y + 1), (x - 1, y) )
minoPos T 3 (x, y) = ( (x, y), (x, y + 1), (x - 1, y), (x, y - 1) )
minoPos T _ (x, y) = ( (x, y), (x - 1, y), (x, y - 1), (x + 1, y) )

-- S Positions
minoPos S 1 (x, y) = ( (x, y), (x, y - 1), (x + 1, y), (x + 1, y + 1) )
minoPos S 2 (x, y) = ( (x, y), (x + 1, y), (x, y + 1), (x - 1, y + 1) )
minoPos S 3 (x, y) = ( (x, y), (x, y + 1), (x - 1, y), (x - 1, y - 1) )
minoPos S _ (x, y) = ( (x, y), (x - 1, y), (x, y - 1), (x + 1, y - 1) )

-- Z Positions
minoPos Z 1 (x, y) = ( (x, y), (x + 1, y - 1), (x + 1, y), (x, y + 1) )
minoPos Z 2 (x, y) = ( (x, y), (x + 1, y + 1), (x, y + 1), (x - 1, y) )
minoPos Z 3 (x, y) = ( (x, y), (x - 1, y + 1), (x - 1, y), (x, y - 1) )
minoPos Z _ (x, y) = ( (x, y), (x - 1, y - 1), (x, y - 1), (x + 1, y) )

-- J Positions
minoPos J 1 (x, y) = ( (x, y), (x, y - 1), (x + 1, y - 1), (x, y + 1) )
minoPos J 2 (x, y) = ( (x, y), (x + 1, y), (x + 1, y + 1), (x - 1, y) )
minoPos J 3 (x, y) = ( (x, y), (x, y + 1), (x - 1, y + 1), (x, y - 1) )
minoPos J _ (x, y) = ( (x, y), (x - 1, y), (x - 1, y - 1), (x + 1, y) )

-- L Positions
minoPos L 1 (x, y) = ( (x, y), (x, y - 1), (x + 1, y + 1), (x, y + 1) )
minoPos L 2 (x, y) = ( (x, y), (x + 1, y), (x - 1, y + 1), (x - 1 ,y) )
minoPos L 3 (x, y) = ( (x, y), (x, y + 1), (x - 1, y - 1), (x, y - 1) )
minoPos L _ (x, y) = ( (x, y), (x - 1, y), (x + 1, y - 1), (x + 1, y) )

-- used to determine kick position to check during rotation
kick :: Mino -> Int -> Int -> [Coord]
-- I Wall Kick Data
kick I 0 1 = [ ( 0, 0), (-2, 0), ( 1, 0), (-2, 1), ( 1,-2) ]
kick I 1 0 = [ ( 0, 0), ( 2, 0), (-1, 0), ( 2,-1), (-1, 2) ]
kick I 1 2 = [ ( 0, 0), (-1, 0), ( 2, 0), (-1,-2), ( 2, 1) ]
kick I 2 1 = [ ( 0, 0), ( 1, 0), (-2, 0), ( 1, 2), (-2,-1) ]
kick I 2 3 = [ ( 0, 0), ( 2, 0), (-1, 0), ( 2,-1), (-1, 2) ]
kick I 3 2 = [ ( 0, 0), (-2, 0), ( 1, 0), (-2, 1), ( 1,-2) ]
kick I 3 0 = [ ( 0, 0), ( 1, 0), (-2, 0), ( 1, 2), (-2,-1) ]
kick I 0 3 = [ ( 0, 0), (-1, 0), ( 2, 0), (-1,-2), ( 2, 1) ]

-- J, L, S, T, Z Wall Kick Data
kick _ 0 1 = [ ( 0, 0), (-1, 0), (-1,-1), ( 0, 2), (-1, 2) ]
kick _ 1 0 = [ ( 0, 0), ( 1, 0), ( 1, 1), ( 0,-2), ( 1,-2) ]
kick _ 1 2 = [ ( 0, 0), ( 1, 0), ( 1, 1), ( 0,-2), ( 1,-2) ]
kick _ 2 1 = [ ( 0, 0), (-1, 0), (-1,-1), ( 0, 2), (-1, 2) ]
kick _ 2 3 = [ ( 0, 0), ( 1, 0), ( 1,-1), ( 0, 2), ( 1, 2) ]
kick _ 3 2 = [ ( 0, 0), (-1, 0), (-1, 1), ( 0,-2), (-1,-2) ]
kick _ 3 0 = [ ( 0, 0), (-1, 0), (-1, 1), ( 0,-2), (-1,-2) ]
kick _ 0 3 = [ ( 0, 0), ( 1, 0), ( 1,-1), ( 0, 2), ( 1, 2) ]


-- Checks if a square coordinate is legal
isLegal :: Coord -> Bool
isLegal (x, y) = left && right && floor where
    left = x > -1
    right = x < 10
    floor = y < 24

checkPos :: MinoPos -> Bool
checkPos (a, b, c, d) = isLegal a && isLegal b && isLegal c && isLegal d

-- Move tetromino by coorinate shift
shift :: MinoPos -> Coord -> MinoPos
shift ((ax, ay), (bx, by), (cx, cy), (dx, dy)) (x, y) = (a, b, c, d) where
    a = (ax + x, ay + y)
    b = (bx + x, by + y)
    c = (cx + x, cy + y)
    d = (dx + x, dy + y)

-- Rotates Tetromino
rotate :: Mino -> Int -> Int -> MinoPos -> (Int, MinoPos)
rotate mino from to position = newPosition where
    (aa, _, _, _) = position
    a = if mino == I then rotateI from to aa else aa
    -- Rotates tetromino
    rotated = minoPos mino to a
    -- Kick positions
    kicks = kick mino from to
    -- Shifted rotations to check
    kicked = map (shift rotated) kicks
    newPosition = findKick from to kicked position

-- Special Rotation for I blocks
rotateI :: Int -> Int -> Coord -> Coord
rotateI 0 1 (x, y) = (x + 2, y - 1)
rotateI 1 0 (x, y) = (x - 2, y + 1)
rotateI 1 2 (x, y) = (x + 1, y + 2)
rotateI 2 1 (x, y) = (x - 1, y - 2)
rotateI 2 3 (x, y) = (x - 2, y + 1)
rotateI 3 2 (x, y) = (x + 2, y - 1)
rotateI 3 0 (x, y) = (x - 1, y - 2)
rotateI 0 3 (x, y) = (x + 1, y + 2)

-- Moves Tetromino
move :: MinoPos -> Coord -> MinoPos
move position movement = newPosition where
    -- Shift tetromino
    shifted = shift position movement
    newPosition = if (checkPos shifted) then shifted else position

findKick :: Int -> Int -> [MinoPos] -> MinoPos -> (Int, MinoPos)
findKick from _ [] oldPosition = (from, oldPosition)
findKick from to (newPosition: rest) oldPosition = if checkPos newPosition then (to, newPosition) else findKick from to rest oldPosition

nextTetromino :: StdGen -> (Tetromino, StdGen)
nextTetromino randGen = (tetromino (toEnum rand :: Mino), newGen)
    where (rand, newGen) = randomR (0, 6) randGen
