module Types where
import Graphics.Gloss.Data.Color
import Data.Matrix

import Graphics.Gloss

data Tetris = Tetris {
    xs :: Float,
    piece :: Tetromino,
    gameBoard :: Matrix Cell
}

-- Tetromino Shape/Type
data Mino = I | O | T | S | Z | J | L deriving Enum

-- Tetromino - a Tetris piece
data Tetromino = Tetromino {
    mino :: Mino,
    rotation :: Int,
    position :: MinoPos
}

-- (x, y) coordinate of a cell in the play area
type Coord = (Int, Int)

-- the full position of a tetromino
type MinoPos = (Coord, Coord, Coord, Coord)

data Cell = Cell {
    x :: Int,
    y :: Int,
    col :: Color,
    occ :: Bool
}
