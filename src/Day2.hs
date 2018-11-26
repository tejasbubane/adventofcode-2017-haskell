module Day2 (checksum1, checksum2) where

import Data.List (find)

checksum :: ([Int] -> Int) -> [[Int]] -> Int
checksum f = sum . map f

rowOp1 :: [Int] -> Int
rowOp1 xs = maximum xs - minimum xs

checksum1 :: [[Int]] -> Int
checksum1 = checksum rowOp1

checkMod :: Int -> Int -> Bool
checkMod x y = x `mod` y == 0 || y `mod` x == 0

rowOp2 :: [Int] -> Int
rowOp2 [] = 0
rowOp2 (x:xs) =
  case find (\y -> checkMod x y) xs of
    Just a -> if a > x then a `div` x else x `div` a
    Nothing -> rowOp2 xs

checksum2 :: [[Int]] -> Int
checksum2 = checksum rowOp2
