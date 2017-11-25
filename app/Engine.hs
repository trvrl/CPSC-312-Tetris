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
-- start = putStrLn (boolToString (checkRow (getRow 1 initBoard)))
-- start = putStrLn (boolToString (checkNthRow 4 initBoard4))

-- start = putStrLn (cellTupleToString (getHighestSquareInColumn (getCol 1 initBoard2)))
-- start = putStrLn (cellTuple3ToString (getHighestSquareBelowTetromino 1 5 initBoard4))
-- start = putStrLn (arrayCellTuple3ToString (getAllHighestSquaresBelowTetromino 1 5 initBoard4))
-- start = putStrLn (showinteger (checkNRows initBoard4 24))
-- start = checkRow (getRow 1 initBoard)
-- start = putStrLn (prettyMatrix initBoard4) -- (copyElementDown 2 3 initBoard4 initEmptyBoard))
-- start = putStrLn (prettyMatrix (clearTopRow initBoard7))
start = putStrLn (prettyMatrix (clearFilledRows height initBoard4))
-- start = putStrLn (prettyMatrix initBoard24)
-- start = putStrLn (prettyMatrix (iterateRow 21 height initBoard24 initEmptyBoard))
-- start = putStrLn (prettyMatrix (iterateRowCopy2 24 width initBoard24 initEmptyBoard))
-- start = putStrLn (prettyMatrix (copyEntireRow 24 3 initBoard24 initEmptyBoard))
-- start = putStrLn (prettyMatrix (iterateRowCopy 5 width initBoard4 initEmptyBoard copyEntireRow))
-- start = putStrLn(show (updateScore initBoard4))
background = greyN 0.8
-- start = putStrLn (boolToString (gameLost initBoard4))
steps = 2
-- use getRow for getting a particular row
-- getMatrixAsVector might be helpful for showing the matrix
initBoard = matrix height width (\(y1,x1) -> Cell{ x = (fromIntegral (x1*squareSizeT-squareSizeT)), y = (fromIntegral (y1*squareSizeT-squareSizeT)), col = black, occ = True})
initBoard2 = setElem Cell{x = 33, y = 44, col = black, occ = False} (5,1) initBoard
initBoard3 = setElem Cell{x = 77, y = 88, col = black, occ = False} (5,2) initBoard2
initBoard4 = setElem Cell{x = 99, y = 11, col = black, occ = False} (4,3) initBoard3

initBoard5 = iterateRow 24 height initBoard4 initEmptyBoard
initBoard6 = iterateRow 24 height initBoard5 initEmptyBoard
initBoard7 = iterateRow 24 height initBoard6 initEmptyBoard
initBoard8 = iterateRow 24 height initBoard7 initEmptyBoard
initBoard9 = iterateRow 24 height initBoard8 initEmptyBoard
initBoard10 = iterateRow 24 height initBoard9 initEmptyBoard
initBoard11 = iterateRow 24 height initBoard10 initEmptyBoard
initBoard12 = iterateRow 24 height initBoard11 initEmptyBoard
initBoard13 = iterateRow 24 height initBoard12 initEmptyBoard
initBoard14 = iterateRow 24 height initBoard13 initEmptyBoard
initBoard15 = iterateRow 24 height initBoard14 initEmptyBoard
initBoard16 = iterateRow 24 height initBoard15 initEmptyBoard
initBoard17 = iterateRow 24 height initBoard16 initEmptyBoard
initBoard18 = iterateRow 24 height initBoard17 initEmptyBoard
initBoard19 = iterateRow 24 height initBoard18 initEmptyBoard
initBoard20 = iterateRow 24 height initBoard19 initEmptyBoard
initBoard21 = iterateRow 24 height initBoard20 initEmptyBoard
initBoard22 = iterateRow 24 height initBoard21 initEmptyBoard
initBoard23 = iterateRow 24 height initBoard22 initEmptyBoard
initBoard24 = iterateRow 24 height initBoard23 initEmptyBoard
initBoard25 = iterateRow 24 height initBoard24 initEmptyBoard
-- newBoard1 = initEmptyBoard
-- initBoard5 = copyElementDown 2 3 initBoard4 newBoard1
-- initBoard6 = copyElementDown 2 2 initBoard4 initBoard5
-- initBoard7 = copyElementDown 2 1 initBoard4 initBoard6
startingGame = Tetris { gameBoard = initBoard }



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

initEmptyBoard :: Matrix Cell
initEmptyBoard = matrix height width (\(y1,x1) -> Cell{ x = (fromIntegral (x1*squareSizeT-squareSizeT)), y = (fromIntegral (y1*squareSizeT-squareSizeT)), col = black, occ = False})

tetris = Tetris {gameBoard = initBoard4}

checkGame :: Tetris -> Tetris
checkGame tetris = Tetris { gameBoard = (clearFilledRows height (gameBoard tetris)) }

gameLost :: Matrix Cell -> Bool
gameLost board = foldr (\ (cell,rowNum,colNum) r -> if rowNum < 5 then True || r else False || r ) False (getAllHighestSquaresInRange 1 width board) 

updateScore :: Matrix Cell -> Int
updateScore board
    | rowsCleared < 4 = 100 * rowsCleared
    | rowsCleared >= 4 = 200 * rowsCleared
    where rowsCleared = length(checkNRows height board)

clearFilledRows :: Int -> Matrix Cell -> Matrix Cell
clearFilledRows row board
    | (row == 1) && (checkNthRow row board) = clearTopRow board
    | (row == 1) && not (checkNthRow row board) = board
    | checkNthRow row board = clearFilledRows row (iterateRow row height board initEmptyBoard)
    | otherwise  = clearFilledRows (row-1) board

clearTopRow :: Matrix Cell -> Matrix Cell
clearTopRow board = emptyRow 1 board

emptyRow :: Int -> Matrix Cell -> Matrix Cell
emptyRow n board = mapRow (\ _ cell -> Cell{ x = (x cell), y = (y cell), col = black, occ = False}) n board

iterateRow :: Int -> Int -> Matrix Cell -> Matrix Cell -> Matrix Cell
iterateRow startInt totalInt oldBoard newBoard
    | startInt == 1 = newBoard
    | startInt <= totalInt = iterateRow startInt (totalInt-1) oldBoard (iterateRowCopy totalInt width oldBoard newBoard copyEntireRow)
    | otherwise = iterateRow (startInt-1) (totalInt-1) oldBoard (iterateRowCopy startInt width oldBoard newBoard copyElementDown)

iterateRowCopy :: Int -> Int -> Matrix Cell -> Matrix Cell -> (Int -> Int -> Matrix Cell -> Matrix Cell -> Matrix Cell) -> Matrix Cell 
iterateRowCopy rowNum colNum oldBoard newBoard fn
    | colNum == 0 = newBoard
    | otherwise = iterateRowCopy rowNum (colNum-1) oldBoard (fn rowNum colNum oldBoard newBoard) fn

{-
-- (Int -> Int -> Matrix Cell -> Matrix Cell -> Matrix Cell) ->
iterateRowCopy :: Int -> Int -> Matrix Cell -> Matrix Cell ->  Matrix Cell 
iterateRowCopy rowNum colNum oldBoard newBoard --fn
    | colNum == 0 = newBoard
    | otherwise = iterateRowCopy rowNum (colNum-1) oldBoard (copyElementDown rowNum colNum oldBoard newBoard) -- fn

iterateRowCopy2 :: Int -> Int -> Matrix Cell -> Matrix Cell ->  Matrix Cell 
iterateRowCopy2 rowNum colNum oldBoard newBoard --fn
    | colNum == 0 = newBoard
    | otherwise = iterateRowCopy2 rowNum (colNum-1) oldBoard (copyEntireRow rowNum colNum oldBoard newBoard) -- fn
-}

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

instance Show Cell where
    show (Cell x y col occ) = show x ++ " " ++ show y ++ " "{- ++ show col ++ " "-} ++ show occ

cellTupleToString (cell, n) = (show cell) ++ " " ++ show n

arrayCellTuple3ToString tuples = if not (null tuples) then (cellTuple3ToString (head tuples)) ++ (arrayCellTuple3ToString (tail tuples)) else ""
cellTuple3ToString (cell, n, k) = (show cell) ++ " " ++ show n ++ " " ++ show k ++ " "
showinteger ints = if null ints then "" else show(head ints) ++ " " ++ showinteger(tail ints)
boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"
