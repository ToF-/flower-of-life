module Geometry
    where

data Pentagon = Pentagon Point Radius Angle
data Arc = Arc { arcOrigin :: Point
               , arcRadius :: Radius
               , arcStartAngle :: Angle
               , arcEndAngle :: Angle }

type Point = (Coord,Coord)
type Angle = Double
type Coord = Double
type Radius= Double

pentagon :: Point -> Radius -> Angle -> Pentagon
pentagon origin radius alpha = Pentagon origin radius alpha

vertices :: Pentagon -> [Point]
vertices (Pentagon (x0,y0) radius alpha) =
    [ ( x0 + radius * cos a
      , y0 + radius * sin a )
    | a <- pentagonAngles alpha]


pentagonAngles :: Angle -> [Angle]
pentagonAngles alpha = take 5 $ iterate (+ (2 * pi / 5)) alpha

arcs :: Pentagon -> [Arc]
arcs h@(Pentagon (x0,y0) radius alpha) = zipWith arc (vertices h) (pentagonAngles alpha)
    where
        arc (x,y) phi = Arc { arcOrigin = (x,y)
                            , arcRadius = radius * sqrt ((5 - (sqrt 5)) / 2)
                            , arcStartAngle = phi + 7*pi/10
                            , arcEndAngle = phi + 13*pi/10 }

distance :: Point -> Point -> Double
distance (x0,y0) (x1,y1) = sqrt (dx * dx + dy * dy)
    where
        dx = x0 - x1
        dy = y0 - y1
