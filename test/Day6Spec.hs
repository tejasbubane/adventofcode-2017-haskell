module Day6Spec where

import Test.Hspec
import Day6
import Data.HashMap
import Data.Array

exampleInput :: Array Int Int
exampleInput = listArray (1, 4) [0, 2, 7, 0]

puzzleInput :: Array Int Int
puzzleInput = listArray (1, 16) [10, 3, 15, 10, 5, 15, 5, 15, 9, 2, 5, 8, 5, 2, 3, 6]

specs :: SpecWith ()
specs = describe "Day 6" $ do
  describe "part 1 - steps to reach loop" $ do
    it "works for example input" $ do
      fst (checkLoop exampleInput empty) `shouldBe` 5
    it "works for puzzle input" $ do
      fst (checkLoop puzzleInput empty) `shouldBe` 14029

  describe "part 2 - loop cycle length" $ do
    it "works for example input" $ do
      snd (checkLoop exampleInput empty) `shouldBe` 4
    it "works for puzzle input" $ do
      snd (checkLoop puzzleInput empty) `shouldBe` 2765
