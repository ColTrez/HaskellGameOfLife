module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.List
import ConstantDeclarations
import RenderingFunctions

main :: IO ()
main = simulate window black simulationSpeed initialConfiguration render nextGeneration
    where
        window :: Display
        window = InWindow "Conway's Game of Life" (screenSize, screenSize) (windowOffset, windowOffset)
        
        initialConfiguration :: [Pos]
        initialConfiguration = [(4,2),(2,3),(4,3),(3,4),(4,4)]
        
        -- The simulate function from Gloss requires the iterating function to take a Viewport and a Float
        -- even if you don't do anything with them (here we just throw them away)
        nextGeneration :: ViewPort -> Float -> [Pos] -> [Pos]
        nextGeneration _ _ ps = nextGen ps

--Life functions
-- Finds the neighbors of any given position
neighbors :: Pos -> [Pos]
neighbors (x,y) = map wrap [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),
                            (x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
                    where
                       wrap :: Pos -> Pos
                       wrap (x,y) = (((x-1) `mod` screenSize) + 1, ((y-1) `mod` screenSize) + 1)

-- Cells that survive to the next generation are those that have 2 or 3 neighboring live cells
surviving :: [Pos] -> [Pos]
surviving livings = survivors 2 ++ survivors 3
                where
                    survivors n = [p | p <- livings, (length (intersect livings (neighbors p))) == n]

-- Cells that are born in the next generation are those dead cells that have precisely 3 live neighbors
beingBorn :: [Pos] -> [Pos]
beingBorn ps = [p | p <- nub (concat (map neighbors ps)),
                                     notElem p ps, length (intersect ps (neighbors p)) == 3]

-- Creates the next generation from the list of surviving cells and the list of cells that will be born
nextGen :: [Pos] -> [Pos]
nextGen livings = surviving livings ++ beingBorn livings
