module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.Word
import Data.ByteString (ByteString, pack)
import Data.List

main :: IO ()
main = simulate window black 1 initialConfiguration render nextGeneration
    where
        window :: Display
        window = InWindow "Conway's Game of Life" (screenSize, screenSize) (20, 20)
        
        initialConfiguration :: [Pos]
        initialConfiguration = map (\(x,y) -> (x+4,y+4)) [(4,2),(2,3),(4,3),(3,4),(4,4)]
        
        scaleModel :: [Pos] -> [Pos]
        scaleModel ps = map (\(x,y) -> (x*8,y*8)) ps

        render :: [Pos] -> Picture
        render ps = pictures (map (\(x,y) -> translate  (x-404) (404-y) cell) (toFloats $ scaleModel ps))
        
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
beingBorn livings = [p | p <- candidates, length (nub (intersect candidates (neighbors p))) == 3]
                    where
                        candidates = (nub (concat $ map neighbors livings)) \\ livings

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

