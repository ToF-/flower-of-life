{-# LANGUAGE OverloadedStrings #-}
module Main where
import Graphics.Blank

{-
   draw the pentagon, joining the vertices with lines
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
    mapM_ (\((x1,y1),(x2,y2)) -> do
        moveTo (x1,y1)
        lineTo (x2,y2)
        )
        $ zip pentagonVertices ((tail pentagonVertices) ++ [head pentagonVertices])
    stroke ()

