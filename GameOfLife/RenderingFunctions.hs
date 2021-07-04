module RenderingFunctions
    ( render
    , scaleModel
    , toFloats
    , floatMod
    , cell) where

import Graphics.Gloss
import ConstantDeclarations

render :: [Pos] -> Picture
render ps = pictures (map (\(x,y) -> translate  (floatMod 400 (x-400)) (floatMod 400 (400-y)) cell) (toFloats $ scaleModel ps))

scaleModel :: [Pos] -> [Pos]
scaleModel ps = map (\(x,y) -> (x*cellSize,y*cellSize)) ps

toFloats :: [Pos] -> [(Float,Float)]
toFloats ps = map (\(x,y) -> (fromIntegral x, fromIntegral y)) ps

floatMod :: Float -> Float -> Float
floatMod n val | val > n     = floatMod n (val - (2 * n))
               | val < (-n)  = floatMod n (val + (2 * n))
               | otherwise   = val

cell :: Picture
cell = (color (dark green) $ rectangleSolid cellSizeFloat cellSizeFloat)
