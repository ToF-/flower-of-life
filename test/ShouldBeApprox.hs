module ShouldBeApprox
    where
import Test.Hspec

shouldBeApprox :: (HasCallStack, Show a, Eq a, Ord a, Floating a) => a -> a -> Expectation
a `shouldBeApprox` b = abs (a - b) `shouldSatisfy` (< 0.000001)
