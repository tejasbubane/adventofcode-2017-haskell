module Day5 where

import Data.Array

steps :: Array Int Int -> Int
steps = stepRec 1 0 where
  stepRec idx count arr =
    if idx > length arr
    then count
    else stepRec (idx + arr!idx) (count + 1) (arr//[(idx, arr!idx + 1)])
