module Main where

import Graphics.Gloss
import Data.Word
import Data.ByteString (ByteString, pack)

main :: IO ()
main = display window background drawing
    where
        window = InWindow "Conway's Game of Life" (800, 800) (20, 20)
        background = black
        drawing = frame

frame :: Picture
frame = bitmapOfByteString 800 800 (BitmapFormat TopToBottom PxRGBA) bitmapData True

livePixel :: [Word8]
livePixel = [0,128,16,128]

deadPixel :: [Word8]
deadPixel = [0,0,0,64]

bitmapData :: ByteString
bitmapData = pack $ take 6400000 (cycle livePixel)


