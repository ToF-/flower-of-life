{-# LANGUAGE OverloadedStrings #-}
module Main where
import Graphics.Blank
import Data.Text (pack)
import Geometry

{-
   draw arcs joining every two vertices of the pentagon
-}


type Level = Integer
flower :: Level -> Pentagon -> Canvas ()
flower 0 p =
    mapM_ (\a -> do
        let (x,y) = arcOrigin a
            radius = arcRadius a
            start  = arcStartAngle a
            end    = arcEndAngle a
        beginPath ()
        arc (x, y, radius, start, end, False)
        stroke ()
        closePath ()) (arcs p)
flower 1 p = do
    flower 0 p
    flower 0 $ (outerPentagons p) !! 0
    flower 0 $ (outerPentagons p) !! 4
flower 2 p = do
    flower 0 p
    mapM_ (\p -> do flower 1 p) (outerPentagons p)

flowerOfLife :: Canvas ()
flowerOfLife = flower 2 (pentagon (600,600) 100 0)

main = blankCanvas 3000 $ \ context -> send context flowerOfLife


