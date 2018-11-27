module Day3Spec (specs) where

import Prelude hiding (Right, Left)
import Test.Hspec
import Day3

specs :: SpecWith ()
specs =  describe "Day 3" $ do
  describe "part 1 - steps" $ do
    it "works for 1" $ do
      steps 1 `shouldBe` 0
    it "works for 2" $ do
      steps 2 `shouldBe` 1
    it "works for 3" $ do
      nextDir (1, 1) Up `shouldBe` Left
      nextCoordinate (1, 1) Left `shouldBe` (0, 1)
      steps 3 `shouldBe` 2
    it "works for 4" $ do
      nextDir (0, 1) Left `shouldBe` Left
      nextCoordinate (0, 1) Left `shouldBe` (-1, 1)
      steps 4 `shouldBe` 1
    it "works for 5" $ do
      nextDir (-1, 1) Left `shouldBe` Down
      nextCoordinate (-1, 1) Down `shouldBe` (-1, 0)
      steps 5 `shouldBe` 2
    it "works for 7" $ do
      nextDir (-1, -1) Down `shouldBe` Right
      nextCoordinate (-1, -1) Right `shouldBe` (0, -1)
      steps 7 `shouldBe` 2
    it "works for 9" $ do
      nextDir (1, -1) Right `shouldBe` Right -- stay there
      nextCoordinate (1, -1) Right `shouldBe` (2, -1)
      nextDir (2, -1) Right `shouldBe` Up -- stay there
      nextCoordinate (2, -1) Up `shouldBe` (2, 0)
      steps 9 `shouldBe` 2
    it "works for 10" $ do
      steps 10 `shouldBe` 3
    it "works for 12" $ do
      steps 12 `shouldBe` 3
    it "works for 23" $ do
      nextDir (-1, -2) Right `shouldBe` Right -- stay there
      nextCoordinate (-1, -2) Right `shouldBe` (0, -2)
      steps 23 `shouldBe` 2
    it "works for 1024" $ do
      steps 1024 `shouldBe` 31
    it "works for puzzle input" $ do
      steps 347991 `shouldBe` 480
