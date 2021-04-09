{-# LANGUAGE OverloadedStrings #-}
module Main where
import Graphics.Blank

{-
   draw 5 small squares on the vertices of a pentagon
   one of the edge of the pentagon should be parallel with x axis
-}

x0 = 300
y0 = 300
r = 100
s = 2 * pi / 5
pentagonVertices = [( x0 + r * cos (angle i)
                    , y0 + r * sin (angle i) )
                    |  i <- [0..4] ]
    where angle n = n * s - pi / 2

main = blankCanvas 3000 $ \ context -> do
  send context $ do
    mapM_ (\(x,y) -> rect (x,y,10,10)) pentagonVertices
    stroke ()

