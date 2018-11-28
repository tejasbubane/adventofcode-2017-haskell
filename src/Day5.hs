module Day5 where

import Data.Array

stepRec :: (Int -> Int) -> Int -> Int -> Array Int Int -> Int
stepRec offsetChange idx count offsets =
  if idx > length offsets
  then count
  else
    let offset = offsets!idx in
      stepRec offsetChange
         (idx + offset)
         (count + 1)
         (offsets//[(idx, offsetChange offset)])

steps1 :: Array Int Int -> Int
steps1 = stepRec (+1) 1 0

steps2 :: Array Int Int -> Int
steps2 = stepRec (\x -> if x > 2 then (x - 1) else (x + 1)) 1 0
