module Graphical where

import Types
import Engine
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix
import System.FilePath
import System.Directory

start :: IO ()
start = do
    currDir <- getCurrentDirectory
    logo <- loadBMP $ currDir ++ [pathSeparator] ++ "tetris_logo.bmp"
    play window background steps initial (toPicture logo) eventHandler step

-- Spacing
space = 2
squareSize :: Float
squareSize = 30
padding = squareSize * 5

-- Window dimensions
vh :: Int
vh = round squareSize * 22
vw :: Int
vw = round squareSize * 23

-- Offset window contents - lines up (0, 0) with bottom left
dx :: Float
dx = fromIntegral vw / (-2)
dy :: Float
dy = fromIntegral vh / (-2)

-- Play area offset
xOffset = 1 * squareSize
yOffset = 1 * squareSize

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
background = white
board_background = translate xOffset yOffset $ rectangle (10 * squareSize) (24 * squareSize)

-- Spawn overlay
spawn :: Picture
spawn = translate (xPixel 0) (yPixel 3) $ color white $ rectangle (squareSize * 10) (squareSize * 4)

-- Creates picture from world
toPicture :: Picture -> Tetris -> Picture
toPicture logo tetris = pictures translated where
    ps = 
        [ board_background
        , drawBoard $ gameBoard tetris
        , drawTetromino $ piece tetris
        , drawScore $ points tetris
        , drawLogo logo
        , drawLevel $ rows tetris
        , spawn
        , drawMode (mode tetris) (state tetris)
        ]
    translated = map (translate dx dy) ps

-- | Creates picture from tetromino
drawTetromino :: Tetromino -> Picture
drawTetromino (Tetromino mino rotation position) = pictures squares where
    positions = posToList position
    squares = map square positions
    square (x, y) = color (minoColor mino) (toSquare (x, y))

-- | Creates Score
drawScore :: Int -> Picture
drawScore points = translate (12 * squareSize) (14 * squareSize) $ pictures [box, score] where
    score = translate (0.25 * squareSize) (0.25 * squareSize) $ color white $ scale (0.015 * squareSize) (0.015 * squareSize) $ text $ show points
    box = rectangle (10 * squareSize) (2 * squareSize)
    
-- | Creates Level
drawLevel :: Int -> Picture
drawLevel rows = translate (12 * squareSize) (10 * squareSize) $ pictures [box, score] where
    level = 1 + div rows 10
    score = translate (0.25 * squareSize) (0.25 * squareSize) $ color white $ scale (0.015 * squareSize) (0.015 * squareSize) $ text $ (id "Level " ++ show level)
    box = color green $ rectangle (10 * squareSize) (2 * squareSize)

-- | Creates prompts for when the game is paused or the game is over
drawMode :: Mode -> State -> Picture
drawMode Play _ = Blank
drawMode Pause Lost = translate 0 (13 * squareSize) $ pictures [box, msg] where
    msg = translate (0.25 * squareSize) (0.25 * squareSize) $ color white $ scale (0.01 * squareSize) (0.01 * squareSize) $ text $ show "You Lost. Press r to play again."
    box = color red $ rectangle (fromIntegral vw) (2 * squareSize)
drawMode Pause _ = translate 0 (13 * squareSize) $ pictures [box, msg] where
    msg = translate (0.25 * squareSize) (0.25 * squareSize) $ color white $ scale (0.015 * squareSize) (0.015 * squareSize) $ text $ show "Paused..."
    box = color red $ rectangle (fromIntegral vw) (2 * squareSize)

-- | Creates picture from input logo
drawLogo :: Picture -> Picture
drawLogo logo = translate (17 * squareSize) (19 * squareSize) $ scale (0.01 * squareSize) (0.01 * squareSize) logo

drawBoard :: Matrix Cell -> Picture
drawBoard board = pictures squares where
    cells = toList board
    squares = map square cells
    square (Cell x y col occ) = color col (toSquare (x, y))

toSquare :: Coord -> Picture
toSquare (x, y) = translate (xPixel x) (yPixel y) (square squareSize)

eventHandler :: Event -> Tetris -> Tetris
-- Rotate Right (CW)
eventHandler (EventKey (Char 'd') Down _ _ )  tetris @ Tetris { mode = Play } =
    tetris { piece = rotateRight (piece tetris) (gameBoard tetris) }

-- Rotate Left (CCW)
eventHandler (EventKey (Char 's') Down _ _ )  tetris @ Tetris { mode = Play } =
    tetris { piece = rotateLeft (piece tetris) (gameBoard tetris) }
    
-- Move Right
eventHandler (EventKey (SpecialKey KeyRight) Down _ _ ) tetris @ Tetris { mode = Play } =
    tetris { piece = moveRight (piece tetris) (gameBoard tetris) }

-- Move Left
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _ ) tetris @ Tetris { mode = Play } =
    tetris { piece = moveLeft (piece tetris) (gameBoard tetris) }

-- Move Down
eventHandler (EventKey (SpecialKey KeyDown) Down _ _ ) tetris @ Tetris { mode = Play } =
    tetris { piece = moveDown (piece tetris) (gameBoard tetris) }
    
-- FOR TESTING - Generate random tetromino
eventHandler (EventKey (Char 'n') Down _ _ ) tetris @ Tetris { mode = Play } =
    tetris { piece = next, randGen = newGen } where (next, newGen) = nextTetromino $ randGen tetris

-- Pauses the game
eventHandler (EventKey (Char 'p') Down _ _ ) tetris @ Tetris { mode = Play } = tetris { mode = Pause }
eventHandler (EventKey (Char 'p') Down _ _ ) tetris @ Tetris { mode = Pause } = tetris { mode = Play }

-- Resets the game
eventHandler (EventKey (Char 'r') Down _ _ ) tetris @ Tetris { mode = Pause } = initial

eventHandler _ tetris = tetris
