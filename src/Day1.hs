module Day1 (captcha) where

import Data.Char (digitToInt)

captcha :: String -> Int
captcha xs = sum matchings where
  matchings = map (digitToInt . fst) $ filter (uncurry (==)) zipAdjacent
  zipAdjacent = zip xs $ (tail xs ++ [head xs])
