module Main where

import Graphics.Gloss
import Data.Word
import Data.ByteString (ByteString, pack)

main :: IO ()
main = display window background drawing
    where
        window = InWindow "Conway's Game of Life" (screenSize, screenSize) (20, 20)
        background = black
        drawing = frame

--matrix definitions
type Pos = (Int,Int) --(row,column)

type Matrix = [[(Pos,Bool)]] --represented as a list of row vectors containing (Position, alive?)

positions :: [Pos]
positions = [(x,y)| x <- [0..99], y <- [0..99]]

rows :: [(Pos,Bool)]
rows = generateRows [(4,2),(2,3),(4,3),(3,4),(4,4)]
--rows = [(pos,bool) | pos <- positions, bool <- cycle [False]]

generateRows :: [Pos] -> [(Pos,Bool)]
generateRows conf = [(pos, if pos `elem` conf then True else False) | pos <- positions] 

testMatrix :: Matrix
testMatrix = makeMatrix rows
            where
               makeMatrix :: [(Pos,Bool)] -> Matrix
               makeMatrix [] = []
               makeMatrix rs = (take 100 rs) : (makeMatrix (drop 100 rs))

repeatRows :: Matrix -> Matrix --copies rows 8 times so that 8 pixels high are drawn instead of 1
repeatRows m = concat $ map (replicate 8) m

testMatrix' :: Matrix
testMatrix' = repeatRows testMatrix

--Constants
screenSize :: Int
screenSize = 800

cellSize :: Int
cellSize = screenSize `div` 10

frame :: Picture
frame = bitmapOfByteString screenSize screenSize (BitmapFormat TopToBottom PxRGBA) bitmapData True

livePixel :: [Word8]
livePixel = [0,128,16,128] --RGBA Green

deadPixel :: [Word8]
deadPixel = [0,0,0,64] --RGBA Black

--cell :: [Word8] -> [[Word8]]
--cell color = take cellSize color
getPixels :: [Word8]
getPixels = concat [if bool == True then (take 32 (cycle livePixel)) else (take 32 (cycle deadPixel)) | bool <- map snd (concat testMatrix')]

bitmapData :: ByteString
--bitmapData = pack $ take ((*4) $ screenSize ^ 2) getPixels
bitmapData = pack $ take ((*4) $ screenSize ^ 2) getPixels


