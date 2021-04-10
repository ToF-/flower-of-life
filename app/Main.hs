{-# LANGUAGE OverloadedStrings #-}
module Main where
import Graphics.Blank
import Data.Text (pack)
import Geometry

{-
   draw arcs joining every two vertices of the pentagon
-}


flower :: Pentagon -> Canvas ()
flower p = mapM_ (\a -> do
    let (x,y) = arcOrigin a
        radius = arcRadius a
        start  = arcStartAngle a
        end    = arcEndAngle a
    beginPath ()
    arc (x, y, radius, start, end, False)
    stroke ()
    closePath ())
    (arcs p)

flowerOfLife :: Canvas ()
flowerOfLife = flower (pentagon (300,300) 100 0)

main = blankCanvas 3000 $ \ context -> send context flowerOfLife


