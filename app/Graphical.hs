module Graphical where

import Types
import Graphics.Gloss

-- Spacing
space = 2

-- Window
window :: Int -> Int -> Display
window x y = InWindow "Tetris" (500, 500) (x, y)

-- Rectangle
rectangle :: Float -> Float -> Picture
rectangle x y = Polygon [ (0, 0), (x, 0), (x, y), (0, y), (0, 0) ]

-- Square
square :: Float -> Picture
square x = rectangle x x

toPicture :: Tetris -> Picture
toPicture (Tetris x) = pictures (i1 ++ [circle 4])

squareSize = 20

data Tetromino = Tetromino {
    rotation :: Int,
    ratations :: [Picture]
}

i1 = -- Horizontal Top I
    [ color cyan (translate (2 * (-squareSize)) 0   (square squareSize))
    , color cyan (translate (-squareSize) 0         (square squareSize))
    , color cyan (translate 0 0                     (square squareSize))
    , color cyan (translate squareSize 0            (square squareSize))
    ]

i2 = -- Vertical Right I
    [ color cyan (translate 0 (2 * (-squareSize))   (square squareSize))
    , color cyan (translate 0 (-squareSize)         (square squareSize))
    , color cyan (translate 0 0                     (square squareSize))
    , color cyan (translate 0 squareSize            (square squareSize))
    ]

i3 = -- Horizontal Bottom I
    [ color cyan (translate 0 (2 * (-squareSize))   (square squareSize))
    , color cyan (translate 0 (-squareSize)         (square squareSize))
    , color cyan (translate 0 0                     (square squareSize))
    , color cyan (translate 0 squareSize            (square squareSize))
    ]
