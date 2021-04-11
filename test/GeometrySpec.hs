module GeometrySpec
    where

import Test.Hspec
import Geometry
import ShouldBeApprox

spec :: SpecWith ()
spec = do
    describe "pentagon vertices" $ do
        it "are at every 2pi/5 on the circle" $ do
            let p = pentagon (0,0) 1 0
            (vertices p) !! 0 `shouldBe` (1, 0)
            (vertices p) !! 1 `shouldBe` (cos (2*pi/5), sin (2*pi/5))
            (vertices p) !! 2 `shouldBe` (cos (4*pi/5), sin (4*pi/5))

        it "are on circle of a given length" $ do
            let p = pentagon (0,0) 10 0
            (vertices p) !! 0 `shouldBe` (10, 0)
            (vertices p) !! 1 `shouldBe` (cos (2*pi/5)*10, sin (2*pi/5)*10)
            (vertices p) !! 2 `shouldBe` (cos (4*pi/5)*10, sin (4*pi/5)*10)
        it "start at a certain angle" $ do
            let p = pentagon (0,0) 1 (pi/2)
            (vertices p) !! 1 `shouldBe` (cos (pi/2+2*pi/5), sin (pi/2+2*pi/5))
            (vertices p) !! 2 `shouldBe` (cos (pi/2+4*pi/5), sin (pi/2+4*pi/5))

    describe "pentagon arcs" $ do
        it "join the next vertex to the previous one" $ do
            let p = pentagon (0,0) 1 0
            let a = (arcs p) !! 0
            arcOrigin a `shouldBe` (vertices p) !! 0
            arcStartAngle a `shouldBe`(pi/2 + pi/5) 
            arcEndAngle a  `shouldBe` (arcStartAngle a + (3*pi/5))
            arcRadius a  `shouldBe`  distance ((vertices p) !! 0) ((vertices p) !! 1)

            let b = (arcs p) !! 1
            arcOrigin b `shouldBe` (vertices p) !! 1
            arcStartAngle b `shouldBe`(2*pi/5 + pi/2 + pi/5) 

    describe "outer pentagons" $ do
        it "are symmetric to the center with respect to edges" $ do
            let r = 1
            let p = pentagon (100,100) r 0
            let (Pentagon (x,y) _ phi) = (outerPentagons p) !! 0
            let vs = vertices p
            let e = distance (vs !! 0) (vs !! 1)
            let t = e / 2
            let s = sqrt (r^2 - t^2)
            distance (x,y) (100,100) `shouldBeApprox` (2 * s)
            x `shouldBeApprox` (100 + 2*s * cos (2*pi / 10))
            y `shouldBeApprox` (100 + 2*s * sin (2*pi / 10))
