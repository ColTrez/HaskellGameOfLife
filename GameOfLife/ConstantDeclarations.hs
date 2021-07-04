module ConstantDeclarations
    ( Pos
    , screenSize
    , windowOffset
    , cartesianOffset
    , cellSize
    , cellSizeFloat
    , simulationSpeed
    ) where

type Pos = (Int,Int) --(x,y)

screenSize :: Int
screenSize = 800

windowOffset :: Int
windowOffset = 20

cartesianOffset :: Int
cartesianOffset = 400

cellSize :: Int
cellSize = 8

-- Gloss uses floats everywhere instead of ints for some dumbass reason
cellSizeFloat :: Float
cellSizeFloat = fromIntegral cellSize

simulationSpeed :: Int
simulationSpeed = 5
