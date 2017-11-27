module Graphical where

import Types
import Engine
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

start = play window background steps initial toPicture eventHandler step

-- Spacing
space = 2
squareSize :: Float
squareSize = 20
padding = squareSize * 5

-- Window dimensions
vh :: Int
vh = round squareSize * 30
vw :: Int
vw = round squareSize * 24

-- Offset window contents - lines up (0, 0) with bottom left
dx :: Float
dx = fromIntegral vw / (-2)
dy :: Float
dy = fromIntegral vh / (-2)

-- Play area offset
xOffset = 2 * squareSize
yOffset = 3 * squareSize

-- Convert coordinates to real pixel
toPixel :: Int -> Float
toPixel x = (fromIntegral x) * squareSize

-- Converts x-coordinate to pixel coordinate
xPixel :: Int -> Float
xPixel x = xOffset + (squareSize * (fromIntegral x))

-- Converts y-coordinate to pixel coordinate
yPixel :: Int -> Float
yPixel y = yOffset + (squareSize * 23) - (squareSize * (fromIntegral y))

-- Window
window :: Display
window = InWindow "Tetris" (vw, vh) (0, 0)

-- Rectangle
rectangle :: Float -> Float -> Picture
rectangle x y = Polygon [ (0, 0), (x, 0), (x, y), (0, y), (0, 0) ]

-- Square
square :: Float -> Picture
square x = rectangle x x

-- Board background
background = greyN 0.8
board_background = translate xOffset yOffset $ rectangle (10 * squareSize) (24 * squareSize)

-- Creates picture from world
toPicture :: Tetris -> Picture
toPicture (Tetris x tetromino) = pictures translated where
    ps = 
        [ board_background
        , drawTetromino tetromino
        , translate xOffset yOffset $ color white $ line [ (0, 400), (200, 400) ]
        , circle 4
        ]
    translated = map (translate dx dy) ps

-- Creates picture from tetromino
drawTetromino :: Tetromino -> Picture
drawTetromino (Tetromino mino rotation (aa, _, _, _)) = pictures squares where
    positions = posToList (minoPos mino rotation aa)
    squares = map overlay positions
    overlay (x, y) = pictures [square, txt, rot] where
        square = toSquare (x, y)
        txt = translate (xPixel x) (yPixel y) (scale 0.1 0.1 (text $ show x))
        rot = text $ show rotation

toSquare :: Coord -> Picture
toSquare (x, y) = translate (xPixel x) (yPixel y) (color cyan (square squareSize))

eventHandler :: Event -> Tetris -> Tetris
-- Rotate Right (CW)
eventHandler (EventKey (Char 'd') Down _ _ ) (Tetris x tetromino) = 
    Tetris x $ rotateRight tetromino

-- Rotate Left (CCW)
eventHandler (EventKey (Char 's') Down _ _ ) (Tetris x tetromino) =
    Tetris x $ rotateLeft tetromino
    
-- Move Right
eventHandler (EventKey (SpecialKey KeyRight) Down _ _ ) (Tetris x tetromino) =
    Tetris x $ moveRight tetromino

-- Move Left
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _ ) (Tetris x tetromino) =
    Tetris x $ moveLeft tetromino

-- Move Down
eventHandler (EventKey (SpecialKey KeyDown) Down _ _ ) (Tetris x tetromino) =
    Tetris x $ moveDown tetromino

-- Move Down
eventHandler (EventKey (SpecialKey KeyUp) Down _ _ ) (Tetris x tetromino) =
    Tetris x $ moveUp tetromino

eventHandler _ tetris = tetris
