module Day2 (checksum) where

rowOp :: [Int] -> Int
rowOp xs = maximum xs - minimum xs

rowCombine :: [Int] -> Int
rowCombine = sum

checksum :: [[Int]] -> Int
checksum = rowCombine . map rowOp
