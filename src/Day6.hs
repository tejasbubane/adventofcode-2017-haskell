module Day6 where

import Data.Array
import Data.Set
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

type Banks = Array Int Int

reallocate :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
reallocate _ _ 0 acc = acc
reallocate idx limit balLeft acc =
  let newIdx = if idx + 1 > limit then 1 else idx + 1
  in
    reallocate newIdx limit (balLeft - 1) ((newIdx, 1):acc)

balance :: Banks -> Set Banks -> Int
balance banks acc =
  let maxEl  = maximum banks
      maxIdx = fromMaybe 0 $ (+1) <$> (elemIndex maxEl $ Data.Array.elems banks)
      (m, n) = bounds banks
      newEls = reallocate maxIdx n maxEl []
      newBanks = accumArray (+) 0 (m, n) (assocs (banks//[(maxIdx, 0)]) ++ newEls)
  in
    if newBanks `member` acc
    then (size acc) + 1
    else balance newBanks $ insert newBanks acc
