module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.List

main :: IO ()
main = simulate window black 5 initialConfiguration render nextGeneration
    where
        window :: Display
        window = InWindow "Conway's Game of Life" (screenSize, screenSize) (20, 20)
        
        initialConfiguration :: [Pos]
        initialConfiguration = [(4,2),(2,3),(4,3),(3,4),(4,4)]
        
        scaleModel :: [Pos] -> [Pos]
        scaleModel ps = map (\(x,y) -> (x*8,y*8)) ps

        render :: [Pos] -> Picture
        render ps = pictures (map (\(x,y) -> translate  (x-400) (400-y) cell) (toFloats $ scaleModel ps))
        
        toFloats :: [Pos] -> [(Float,Float)]
        toFloats ps = map (\(x,y) -> (fromIntegral x, fromIntegral y)) ps

        nextGeneration :: ViewPort -> Float -> [Pos] -> [Pos]
        nextGeneration _ _ ps = nextGen ps

--matrix definitions
type Pos = (Int,Int)--(row,column)

cell :: Picture
cell = (color (dark green) $ rectangleSolid 8 8)

--Life functions
surviving :: [Pos] -> Int -> [Pos]
surviving livings n = [p | p <- livings, (length  (intersect livings (neighbors p))) == n]

beingBorn :: [Pos] -> [Pos]
beingBorn ps = [p | p <- nub (concat (map neighbors ps)), notElem p ps, length (intersect ps (neighbors p)) == 3]

neighbors :: Pos -> [Pos]
neighbors (x,y) = map wrap [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
                    where
                       wrap :: Pos -> Pos
                       wrap (x,y) = (((x-1) `mod` screenSize) + 1, ((y-1) `mod` screenSize) + 1)

nextGen :: [Pos] -> [Pos]
nextGen livings = surviving livings 2 ++ surviving livings 3 ++ beingBorn livings

--Constants
screenSize :: Int
screenSize = 800

cellSize :: Int
cellSize = screenSize `div` 10

