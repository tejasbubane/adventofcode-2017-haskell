module Day6 where

import Data.Array
import Data.HashMap hiding (elems)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

type Banks = Array Int Int

reallocate :: Int -> Int -> Int -> Banks -> Banks
reallocate _ _ 0 acc = acc
reallocate idx limit balLeft acc =
  let newIdx = if idx + 1 > limit then 1 else idx + 1
      l = (Data.Array.!)
  in
    reallocate newIdx limit (balLeft - 1) (acc//[(newIdx, acc`l`newIdx + 1)])

checkLoop :: Banks -> Map [Int] Int -> (Int, Int)
checkLoop banks acc =
  let maxEl  = maximum banks
      bankVals = elems banks
      maxIdx = fromMaybe 0 $ (+1) <$> (elemIndex maxEl $ bankVals)
      newBanks = reallocate maxIdx (length banks) maxEl (banks//[(maxIdx, 0)])
      steps = size acc
      l = (Data.HashMap.!)
  in
    if bankVals `member` acc
    then (steps, steps - acc`l`bankVals)
    else checkLoop newBanks $ insert bankVals steps acc
