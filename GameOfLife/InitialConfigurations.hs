module InitialConfigurations
    ( glider
    , block
    , beehive
    , rPentomino
    , dieHard
    , glidersByTheDozen) where

import ConstantDeclarations

-- README: Configurations are represented as a list of ordered pairs representing the starting
-- live cells. The model grid uses all positive numbers, 0 to 100 x and y, with (0,0) being the
-- top left and (100,100) being the bottom right. I did it this way because I expected Gloss to
-- handle graphics the same way like most graphics libraries in other languages I've used do,
-- but instead Gloss just draws shit on the cartesian plane which was a headache to shift around
-- and get that pacman style screen wrapping to work.

-- TODO: Add some function that generates a random starting configuration

-- Shifts the cells so they start away from the edge of the screen.
-- 50 puts them right about the middle of the window. This shift happens before the
-- rendering functions scale the model to produce the image, so keep the number here
-- between 1 and 100, since the "game" uses a model of a 100 by 100 grid regardless of
-- the screen size or cell size
shift :: Pos -> Pos
shift (x,y) = (x+shiftAmount, y+shiftAmount)
    where
        shiftAmount :: Int
        shiftAmount = 50

-- Still lifes: These don't change. They're boring. Programming them was a waste of time.
block :: [Pos]
block = [(10,10),(10,11),(11,10),(11,11)]

beehive :: [Pos]
beehive = [(3,2),(4,2),(2,3),(5,3),(3,4),(4,4)]

-- more interesting patterns
glider :: [Pos]
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

rPentomino :: [Pos]
rPentomino = [(8,7),(9,8),(7,8),(8,8),(8,9)]

dieHard :: [Pos]
dieHard = map shift [(8,1),(2,2),(3,2),(3,3),(7,3),(8,3),(9,3)]

-- This is the most interesting pattern that I have the patience to hardcode
glidersByTheDozen :: [Pos]
glidersByTheDozen = map shift [(2,2),(3,2),(6,2),(2,3),(6,3),(2,4),(5,4),(6,4)]
