module Year2021Spec (spec) where

import Test.Hspec
import qualified Year2021.Day6 as D6

spec :: Spec
spec = do
  describe "solve1" $ do
    it "solve1 after 18 days, there are a total of 26 fish" $ do
      D6.solve1 18 [3,4,3,1,2] `shouldBe` 26