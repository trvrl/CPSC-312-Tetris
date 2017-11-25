module Engine where

import Types
import Graphical
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix
import qualified Data.Vector as Vector
import Data.Bool

squareSizeT, height, heightEnd, width :: Int
width = 10
height = 24
heightEnd = 24
squareSizeT = 20

-- start = play (window 1920 1080) background steps startingGame toPicture eventHandler step
background = greyN 0.8
steps = 2
startingGame = Tetris { gameBoard = initBoard }
tetris = Tetris {gameBoard = initBoard4}

-- Takes in a Matrix column (as a Vector) and returns the occupied cell with the smallest y value and its index.
getHighestSquareInColumn :: Vector.Vector Cell -> (Cell, Int)
getHighestSquareInColumn vec =  getMinY vec 1

-- Returns the cell and index of the lowest indexed occupied cell
getMinY :: Vector.Vector Cell -> Int -> (Cell,Int)
getMinY vec n = if occ1 then ((Vector.head vec),n) else if n == 24 then (Cell{ x = x1, y = y1, col = col1, occ = occ1}, (height+1)) else getMinY (Vector.tail vec) (n+1)
    where 
        x1 = x (Vector.head vec)
        y1 = fromIntegral ((round (y (Vector.head vec))) + squareSizeT)
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
initEmptyBoard = matrix height width (\(y1,x1) -> Cell{ x = (fromIntegral (x1*squareSizeT-squareSizeT)), y = (fromIntegral (y1*squareSizeT-squareSizeT)), col = black, occ = False})

-- Checks the game for things to be updated
checkGame :: Tetris -> Tetris
checkGame tetris = Tetris { gameBoard = (clearFilledRows height (gameBoard tetris)) }

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


--eventHandler :: Event -> Tetris -> Tetris
--eventHandler (EventKey (SpecialKey KeyUp) _ _ _) (Tetris x) = Tetris (x + 1)
--eventHandler (EventKey (SpecialKey KeyDown) _ _ _) (Tetris x) = Tetris (x - 1)
eventHandler _ tetris = tetris

--step :: Float -> Tetris -> Tetris
step 1 tetris = tetris
--step _ tetris = tetris
