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

main = blankCanvas 3000 $ \ context -> do
  send context $ do
    mapM_ (\a -> rect (x0+r*cos(a), y0+r*sin(a), 10, 10)) [i*s - pi/2 | i<- [0..4]]
    stroke ()

