module Types where

import Graphics.Gloss

data Tetris = Tetris {
    xs :: Float,
    piece :: Tetromino
}

data Mino = I | O | T | S | Z | J | L deriving Enum

data Tetromino = Tetromino {
    mino :: Mino,
    rotation :: Int,
    position :: MinoPos
}

type Coord = (Int, Int)
type MinoPos = (Coord, Coord, Coord, Coord)
