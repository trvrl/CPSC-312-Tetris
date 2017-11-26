module Engine where

import Types
import Data.List
import Data.Maybe

initial = Tetris 50 (tetromino T)

steps :: Int
steps = 2

step :: Float -> Tetris -> Tetris
step _ (Tetris x tetromino) = (Tetris x tetromino)

moveUp :: Tetromino -> Tetromino
moveUp (Tetromino mino rotation minoPos) = Tetromino mino rotation newPos
    where newPos = move minoPos(0, -1)

moveDown :: Tetromino -> Tetromino
moveDown (Tetromino mino rotation minoPos) = Tetromino mino rotation newPos
    where newPos = move minoPos(0, 1)

moveRight :: Tetromino -> Tetromino
moveRight (Tetromino mino rotation minoPos) = Tetromino mino rotation newPos
    where newPos = move minoPos (1, 0)

moveLeft :: Tetromino -> Tetromino
moveLeft (Tetromino mino rotation minoPos) = Tetromino mino rotation newPos 
    where newPos = move minoPos (-1, 0)

rotateRight :: Tetromino -> Tetromino
rotateRight (Tetromino mino from position) = Tetromino mino newRot newPos where 
    to = if (from < 3) then from + 1 else 0
    (newRot, newPos) = rotate mino from to position

rotateLeft :: Tetromino -> Tetromino
rotateLeft (Tetromino mino from position) = Tetromino mino newRot newPos where
    to = if (from > 0) then from - 1 else 3
    (newRot, newPos) = rotate mino from to position

tetromino :: Mino -> Tetromino
tetromino mino = Tetromino mino 0 (initPos mino)

initPos :: Mino -> MinoPos
initPos I = minoPos I 0 (3, 3)
initPos O = minoPos O 0 (4, 2)
initPos mino = minoPos mino 0 (4, 3)

mapPos :: (Coord -> t) -> MinoPos -> (t, t, t, t)
mapPos f (a, b, c, d) = (f a, f b, f c, f d)

posToList :: MinoPos -> [Coord]
posToList (a, b, c, d) = [a, b, c, d]

-- I Positions
minoPos :: Mino -> Int -> Coord -> MinoPos

minoPos I 1 (x, y) = ( (x, y), (x, y + 1), (x, y + 2), (x, y + 3) )
minoPos I 2 (x, y) = ( (x, y), (x - 1, y), (x - 2, y), (x - 3, y) )
minoPos I 3 (x, y) = ( (x, y), (x, y - 1), (x, y - 2), (x, y - 3) )
minoPos I _ (x, y) = ( (x, y), (x + 1, y), (x + 2, y), (x + 3, y) )

-- O Positions
minoPos O _ (x, y) = ( (x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1) )

-- T Positions
minoPos T 1 (x, y) = ( (x, y), (x, y - 1), (x + 1, y), (x, y + 1) )
minoPos T 2 (x, y) = ( (x, y), (x + 1, y), (x, y + 1), (x - 1, y) )
minoPos T 3 (x, y) = ( (x, y), (x, y + 1), (x - 1, y), (x, y - 1) )
minoPos T _ (x, y) = ( (x, y), (x - 1, y), (x, y - 1), (x + 1, y) )

-- S Positions
minoPos S 1 (x, y) = ( (x, y), (x, y - 1), (x + 1, y), (x + 1, y + 1) )
minoPos S 2 (x, y) = ( (x, y), (x + 1, y), (x, y + 1), (x - 1, y + 1) )
minoPos S 3 (x, y) = ( (x, y), (x, y + 1), (x - 1, y), (x - 1, y - 1) )
minoPos S _ (x, y) = ( (x, y), (x - 1, y), (x, y - 1), (x + 1, y - 1) )

-- Z Positions
minoPos Z 1 (x, y) = ( (x, y), (x + 1, y - 1), (x + 1, y), (x, y + 1) )
minoPos Z 2 (x, y) = ( (x, y), (x + 1, y + 1), (x, y + 1), (x - 1, y) )
minoPos Z 3 (x, y) = ( (x, y), (x - 1, y + 1), (x - 1, y), (x, y - 1) )
minoPos Z _ (x, y) = ( (x, y), (x - 1, y - 1), (x, y - 1), (x + 1, y) )

-- J Positions
minoPos J 1 (x, y) = ( (x, y), (x, y - 1), (x + 1, y - 1), (x, y + 1) )
minoPos J 2 (x, y) = ( (x, y), (x + 1, y), (x + 1, y + 1), (x - 1, y) )
minoPos J 3 (x, y) = ( (x, y), (x, y + 1), (x - 1, y + 1), (x, y - 1) )
minoPos J _ (x, y) = ( (x, y), (x - 1, y), (x - 1, y - 1), (x + 1, y) )

-- L Positions
minoPos L 1 (x, y) = ( (x, y), (x, y - 1), (x + 1, y + 1), (x, y + 1) )
minoPos L 2 (x, y) = ( (x, y), (x + 1, y), (x - 1, y + 1), (x - 1 ,y) )
minoPos L 3 (x, y) = ( (x, y), (x, y + 1), (x - 1, y - 1), (x, y - 1) )
minoPos L _ (x, y) = ( (x, y), (x - 1, y), (x + 1, y - 1), (x + 1, y) )


kick :: Mino -> Int -> Int -> [Coord]
-- I Wall Kick Data
kick I 0 1 = [ ( 0, 0), (-2, 0), ( 1, 0), (-2, 1), ( 1,-2) ]
kick I 1 0 = [ ( 0, 0), ( 2, 0), (-1, 0), ( 2,-1), (-1, 2) ]
kick I 1 2 = [ ( 0, 0), (-1, 0), ( 2, 0), (-1,-2), ( 2, 1) ]
kick I 2 1 = [ ( 0, 0), ( 1, 0), (-2, 0), ( 1, 2), (-2,-1) ]
kick I 2 3 = [ ( 0, 0), ( 2, 0), (-1, 0), ( 2,-1), (-1, 2) ]
kick I 3 2 = [ ( 0, 0), (-2, 0), ( 1, 0), (-2, 1), ( 1,-2) ]
kick I 3 0 = [ ( 0, 0), ( 1, 0), (-2, 0), ( 1, 2), (-2,-1) ]
kick I 0 3 = [ ( 0, 0), (-1, 0), ( 2, 0), (-1,-2), ( 2, 1) ]

-- J, L, S, T, Z Wall Kick Data
kick _ 0 1 = [ ( 0, 0), (-1, 0), (-1,-1), ( 0, 2), (-1, 2) ]
kick _ 1 0 = [ ( 0, 0), ( 1, 0), ( 1, 1), ( 0,-2), ( 1,-2) ]
kick _ 1 2 = [ ( 0, 0), ( 1, 0), ( 1, 1), ( 0,-2), ( 1,-2) ]
kick _ 2 1 = [ ( 0, 0), (-1, 0), (-1,-1), ( 0, 2), (-1, 2) ]
kick _ 2 3 = [ ( 0, 0), ( 1, 0), ( 1,-1), ( 0, 2), ( 1, 2) ]
kick _ 3 2 = [ ( 0, 0), (-1, 0), (-1, 1), ( 0,-2), (-1,-2) ]
kick _ 3 0 = [ ( 0, 0), (-1, 0), (-1, 1), ( 0,-2), (-1,-2) ]
kick _ 0 3 = [ ( 0, 0), ( 1, 0), ( 1,-1), ( 0, 2), ( 1, 2) ]


-- Checks if a square coordinate is legal
isLegal :: Coord -> Bool
isLegal (x, y) = left && right && floor where
    left = x > -1
    right = x < 10
    floor = y < 24

checkPos :: MinoPos -> Bool
checkPos (a, b, c, d) = isLegal a && isLegal b && isLegal c && isLegal d

-- Move tetromino by coorinate shift
shift :: MinoPos -> Coord -> MinoPos
shift ((ax, ay), (bx, by), (cx, cy), (dx, dy)) (x, y) = (a, b, c, d) where
    a = (ax + x, ay + y)
    b = (bx + x, by + y)
    c = (cx + x, cy + y)
    d = (dx + x, dy + y)

-- Rotates Tetromino
rotate :: Mino -> Int -> Int -> MinoPos -> (Int, MinoPos)
rotate mino from to position = newPosition where
    (a, _, _, _) = position
    -- Rotates tetromino
    rotated = minoPos mino to a
    -- Kick positions
    kicks = kick mino from to
    -- Shifted rotations to check
    kicked = map (\d -> shift position d) [(0, 0)]
    newPosition = myFind checkPos kicked position
    -- Find the first position that is legal
    -- newPosition = if (checkPos rotated) then (to, rotated) else (from, position)
    -- newPosition = if isJust checked then (to, fromJust checked) else (from, position)

-- Moves Tetromino
move :: MinoPos -> Coord -> MinoPos
move position movement = newPosition where
    -- Shift tetromino
    shifted = shift position movement
    newPosition = if (checkPos shifted) then shifted else position

myFind :: (t -> Bool) -> [t] -> t -> t
myFind pred (h: t) def = if (pred h) then h else myFind pred t
myFind pred [] def = def
