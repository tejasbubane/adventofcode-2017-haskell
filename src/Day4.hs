module Day4 where

import Prelude hiding (Word)
import Data.List (find, sort)

type Word = String
type Sentence = String

checkPhrase :: (Word -> Word -> Bool) -> [Word] -> Bool
checkPhrase _ []  = True
checkPhrase checkFun (w:ws) =
  case find (checkFun w) ws of
    Nothing -> checkPhrase checkFun ws
    Just _  -> False

noDups :: [Word] -> Bool
noDups = checkPhrase (==)

countValid :: ([Word] -> Bool) -> String -> Int
countValid validFun str = foldr performCount 0 $ map words . lines $ str where
  performCount ws count = if validFun ws then count + 1 else count

countNoDups :: String -> Int
countNoDups = countValid noDups

-- part 2
anagram :: Word -> Word -> Bool
anagram x y = (sort x) == (sort y)

noAnagrams :: [Word] -> Bool
noAnagrams = checkPhrase anagram

countNoAnagrams :: String -> Int
countNoAnagrams = countValid noAnagrams
