module Day1 (captcha1, captcha2) where

import Data.Char (digitToInt)

captcha1 :: String -> Int
captcha1 xs = sum $ matchings zipAdjacent where
  zipAdjacent = zip xs $ (tail xs ++ [head xs])

captcha2 :: String -> Int
captcha2 xs = sum $ matchings zipped where
  (fstHalf, sndHalf) = splitAt ((length xs) `div` 2) xs
  zipped = zip xs $ sndHalf ++ fstHalf

matchings :: [(Char, Char)] -> [Int]
matchings xs = map (digitToInt . fst) $ filter (uncurry (==)) xs
