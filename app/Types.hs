module Types where
import Graphics.Gloss.Data.Color
import Data.Matrix

data Tetris = Tetris {
    gameBoard :: Matrix Cell
}

data Cell = Cell {
    x :: Float,
    y :: Float,
    col :: Color,
    occ :: Bool
}
