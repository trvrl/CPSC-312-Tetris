module Graphical where

import Types
import Engine
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix

start = play window background steps initial toPicture eventHandler step

-- Spacing
space = 2
squareSize :: Float
squareSize = 30
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
toPicture (Tetris points tetromino board _) = pictures translated where
    ps = 
        [ board_background
        , drawBoard board
        , drawTetromino tetromino
        -- , translate xOffset yOffset $ color white $ line [ (0, 4 * squareSize), (200, 400) ]
        , circle 4
        ]
    translated = map (translate dx dy) ps

-- Creates picture from tetromino
drawTetromino :: Tetromino -> Picture
drawTetromino (Tetromino mino rotation position) = pictures squares where
    positions = posToList position
    squares = map overlay positions
    overlay (x, y) = pictures [square, txt, rot] where
        square = color (minoColor mino) (toSquare (x, y))
        txt = translate (xPixel x) (yPixel y) (scale 0.1 0.1 (text ("(" ++ show x ++ ", " ++ show y ++ ")")))
        rot = text $ show rotation

minoColor :: Mino -> Color
minoColor I = cyan
minoColor O = yellow
minoColor T = violet
minoColor S = green
minoColor Z = red
minoColor J = blue
minoColor L = orange

drawBoard :: Matrix Cell -> Picture
drawBoard board = pictures squares where
    cells = toList board
    squares = map overlay cells
    overlay (Cell x y col occ) = pictures [square, txt] where
        square = color col (toSquare (x, y))
        txt = color white (translate (xPixel x) (yPixel y) (scale 0.1 0.1 (text $ show occ)))

toSquare :: Coord -> Picture
toSquare (x, y) = translate (xPixel x) (yPixel y) (square squareSize)

eventHandler :: Event -> Tetris -> Tetris
-- Rotate Right (CW)
eventHandler (EventKey (Char 'd') Down _ _ ) (Tetris x tetromino board randGen) = 
    Tetris x (rotateRight tetromino) board randGen

-- Rotate Left (CCW)
eventHandler (EventKey (Char 's') Down _ _ ) (Tetris x tetromino board randGen) =
    Tetris x (rotateLeft tetromino) board randGen
    
-- Move Right
eventHandler (EventKey (SpecialKey KeyRight) Down _ _ ) (Tetris x tetromino board randGen) =
    Tetris x (moveRight tetromino) board randGen

-- Move Left
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _ ) (Tetris x tetromino board randGen) =
    Tetris x (moveLeft tetromino) board randGen

-- Move Down
eventHandler (EventKey (SpecialKey KeyDown) Down _ _ ) (Tetris x tetromino board randGen) =
    Tetris x (moveDown tetromino) board randGen

-- Move Down
eventHandler (EventKey (SpecialKey KeyUp) Down _ _ ) (Tetris x tetromino board randGen) =
    Tetris x (moveUp tetromino) board randGen
    
-- FOR TESTING - Generate random tetromino
eventHandler (EventKey (Char 'n') Down _ _ ) (Tetris x _ board randGen) = 
    Tetris x next board newGen where
        (next, newGen) = nextTetromino randGen

eventHandler _ tetris = tetris
