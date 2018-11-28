module Day4 where

import Prelude hiding (Word)
import qualified Data.Set as S

type Word = String
type Sentence = String

checkPhrase :: S.Set Word -> [Word] -> Bool
checkPhrase _ []  = True
checkPhrase s (w:ws)
  | S.notMember w s = checkPhrase (S.insert w s) ws
  | otherwise     = False

countValid :: String -> Int
countValid str = foldr performCount 0 $ map words . lines $ str where
  performCount ws count = if checkPhrase S.empty ws then count + 1 else count
