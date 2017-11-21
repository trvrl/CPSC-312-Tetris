module Engine where

import Types
import Graphical
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

start = play (window 1920 1080) background steps (Tetris 50) toPicture eventHandler step

background = greyN 0.8

steps = 2

eventHandler :: Event -> Tetris -> Tetris
eventHandler (EventKey (SpecialKey KeyUp) _ _ _) (Tetris x) = Tetris (x + 1)
eventHandler (EventKey (SpecialKey KeyDown) _ _ _) (Tetris x) = Tetris (x - 1)
eventHandler _ tetris = tetris

step :: Float -> Tetris -> Tetris
step 1 tetris = tetris
step _ tetris = tetris
