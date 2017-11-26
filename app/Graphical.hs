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

{-
drawTetromino (Tetromino I rotation _ _ _ _) = pictures $ is !! rotation
drawTetromino (Tetromino O rotation _ _ _ _) = pictures $ os !! rotation
drawTetromino (Tetromino T rotation _ _ _ _) = pictures $ ts !! rotation
drawTetromino (Tetromino S rotation _ _ _ _) = pictures $ ss !! rotation
drawTetromino (Tetromino Z rotation _ _ _ _) = pictures $ zs !! rotation
drawTetromino (Tetromino J rotation _ _ _ _) = pictures $ js !! rotation
drawTetromino (Tetromino L rotation _ _ _ _) = pictures $ ls !! rotation
-}

{-
-- Converts coordinates to pixels

one = squareSize
half = squareSize / 2
onehalf = one + half
two = 2 * one

sqr = square squareSize

tl = translate (- onehalf)  half        sqr
tm = translate (- half)     half        sqr
tr = translate half         half        sqr
ml = translate (- onehalf)  (- half)    sqr
mm = translate (- half)     (- half)    sqr
mr = translate half         (- half)    sqr
bl = translate (- onehalf)  (- onehalf) sqr
bm = translate (- half)     (- onehalf) sqr
br = translate half         (- onehalf) sqr

-- I tetromino
is = [ i0, i1, i2, i3 ]
i0 = -- Horizontal Top I
    [ color cyan (translate (- two) 0   sqr)
    , color cyan (translate (- one) 0   sqr)
    , color cyan (translate 0       0   sqr)
    , color cyan (translate one     0   sqr)
    ]

i1 = -- Vertical Right I
    [ color cyan (translate 0 (- two)   sqr)
    , color cyan (translate 0 (- one)   sqr)
    , color cyan (translate 0 0         sqr)
    , color cyan (translate 0 one       sqr)
    ]

i2 = -- Horizontal Bottom I
    [ color cyan (translate (- two) (- one) sqr)
    , color cyan (translate (- one) (- one) sqr)
    , color cyan (translate 0       (- one) sqr)
    , color cyan (translate one     (- one) sqr)
    ]

i3 = -- Vertical Left I
    [ color cyan (translate (- one) (- two) sqr)
    , color cyan (translate (- one) (- one) sqr)
    , color cyan (translate (- one) 0       sqr)
    , color cyan (translate (- one) one     sqr)
    ]


-- O Tetromino
os = [ o0, o0, o0, o0 ]

o0 = -- O
    [ color yellow (translate (- one)   0       sqr)
    , color yellow (translate 0         0       sqr)
    , color yellow (translate (- one)   (- one) sqr)
    , color yellow (translate 0         (- one) sqr)
    ]


-- T Tetromino
ts = [ t0, t1, t2, t3 ]

-- Horizontal Up T
t0 = [ color violet mm, color violet ml, color violet tm, color violet mr ]

-- Vertical Right T
t1 = [ color violet mm, color violet tm, color violet mr, color violet bm ]

-- Horizontal Down T
t2 = [ color violet mm, color violet ml, color violet mr, color violet bm ]

-- Vertical Left T
t3 = [ color violet mm, color violet ml, color violet tm, color violet bm ]


-- S Tetromino
ss = [ s0, s1, s2, s3 ]

-- Horizontal Top S
s0 = [ color green mm, color green ml, color green tm, color green tr ]

-- Vertical Right S
s1 = [ color green mm, color green tm, color green mr, color green br ]

-- Horizontal Bottom S
s2 = [ color green mm, color green bl, color green bm, color green mr ]

-- Vertical Left S
s3 = [ color green mm, color green ml, color green tl, color green bm ]


-- Z Tetromino
zs = [ z0, z1, z2, z3 ]

-- Horizontal Top Z
z0 = [ color red mm, color red tl, color red tm, color red mr ]

-- Vertical Right Z
z1 = [ color red mm, color red tr, color red mr, color red bm ]

-- Horizontal Bottom Z
z2 = [ color red mm, color red br, color red bm, color red ml ]

-- Vertical Left Z
z3 = [ color red mm, color red ml, color red tm, color red bl ]


-- J Tetromino
js = [ j0, j1, j2, j3 ]

-- Horizontal Top J
j0 = [ color blue mm, color blue ml, color blue tl, color blue mr ]

-- Horizontal Top J
j1 = [ color blue mm, color blue tm, color blue tr, color blue bm ]

-- Horizontal Top J
j2 = [ color blue mm, color blue ml, color blue mr, color blue br ]

-- Horizontal Top J
j3 = [ color blue mm, color blue tm, color blue bm, color blue bl ]


-- L Tetromino
ls = [ l0, l1, l2, l3 ]

-- Horizontal Top J
l0 = [ color orange mm, color orange ml, color orange tr, color orange mr ]

-- Horizontal Top J
l1 = [ color orange mm, color orange tm, color orange br, color orange bm ]

-- Horizontal Top J
l2 = [ color orange mm, color orange ml, color orange mr, color orange bl ]

-- Horizontal Top J
l3 = [ color orange mm, color orange tl, color orange tm, color orange bm ]

i_mino = Tetromino I 0 (0, 0) (0, 0) (0, 0) (0, 0)
o_mino = Tetromino O 0 (0, 0) (0, 0) (0, 0) (0, 0)
t_mino = Tetromino T 0 (0, 0) (0, 0) (0, 0) (0, 0)
s_mino = Tetromino S 0 (0, 0) (0, 0) (0, 0) (0, 0)
z_mino = Tetromino Z 0 (0, 0) (0, 0) (0, 0) (0, 0)
j_mino = Tetromino J 0 (0, 0) (0, 0) (0, 0) (0, 0)
l_mino = Tetromino L 0 (0, 0) (0, 0) (0, 0) (0, 0)

-}