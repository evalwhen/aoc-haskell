module Year2023Spec (spec) where

import Test.Hspec
import qualified Year2023.Day12 as D12

spec :: Spec
spec = do
  describe "solve1" $ do
    it "countLine and parseLine" $ do
      D12.countLine "???.###" [1,1,3] `shouldBe` 1
      D12.countLine "?###????????" [3,2,1] `shouldBe` 10
      D12.parseLine "???.### 1,1,3" `shouldBe` Just ("???.###", [1,1,3])