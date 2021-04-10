{-# LANGUAGE OverloadedStrings #-}
module Main where
import Graphics.Blank
import Data.Text (pack)

{-
   draw arcs joining every two vertices of the pentagon
-}

type Angle = Double
type Coord = Double
type Point = (Coord, Coord)
unity = 200
origin :: Point
origin = (unity*2, unity*2)

coordX, coordY :: Point -> Coord
coordX = fst
coordY = snd

distance :: Point -> Point -> Double
distance (x0,y0) (x1,y1) = sqrt (dx * dx + dy * dy)
    where
        dx = x0 - x1
        dy = y0 - y1

r :: Double
r = unity * 0.8

pentagonAngles :: [Angle]
pentagonAngles = take 5 (iterate (+ (2 * pi /5 )) (3 * pi / 2))
startAngles    = map (+ (pi / 2  + pi / 5)) pentagonAngles
pentagonVertices = [(x0 + r * cos a
                    ,y0 + r * sin a)
                      | a <- pentagonAngles]
    where
        x0 = coordX origin
        y0 = coordY origin

edge :: Double
edge = distance (pentagonVertices !! 0) (pentagonVertices !! 1)

pentagonArcs :: [(Coord, Coord, Double, Angle, Angle, Bool)] 
pentagonArcs = zipWith pentagonArc pentagonVertices startAngles
    where
        pentagonArc (xO,yO) angle = (xO, yO, edge, angle, angle + 3 * pi / 5, False)

flowerOfLife :: Canvas ()
flowerOfLife = mapM_ (\args -> beginPath () >> arc args >> stroke () >> closePath ()) pentagonArcs

main = blankCanvas 3000 $ \ context -> send context flowerOfLife


