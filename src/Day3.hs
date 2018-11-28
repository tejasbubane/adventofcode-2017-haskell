module Day3 where

import Prelude hiding (Right, Left)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Direction =
  Up | Down | Left | Right
  deriving (Eq, Show)

type Coordinate = (Int, Int)

nextCoordinate :: Coordinate -> Direction -> Coordinate
nextCoordinate (x, y) Up = (x, y + 1)
nextCoordinate (x, y) Down = (x, y - 1)
nextCoordinate (x, y) Right = (x + 1, y)
nextCoordinate (x, y) Left = (x - 1, y)

-- anti-clockwise motion
changeDir :: Direction -> Direction
changeDir Right = Up
changeDir Up = Left
changeDir Left = Down
changeDir Down = Right

nextDir :: Coordinate -> Direction -> Direction
nextDir (x, y) dir
  | x == y = changeDir dir -- top right, bottom left corners
  | y > 0 && abs x == abs y = changeDir dir -- top left corner
  | y < 0 && x - 1 == abs y = changeDir dir -- one step after bottom right
  | otherwise = dir -- all others including bottom right keep going

steps :: Int -> Int
steps 1 = 0
steps 2 = 1
steps num = traverseMemory 3 ((1, 1), Up) where
  traverseMemory :: Int -> (Coordinate, Direction) -> Int
  traverseMemory i (c@(x, y), dir)
    | i == num  = abs x + abs y
      -- use new direction to calculate next coordinate
    | otherwise = traverseMemory (i + 1) (nCoord, nDir) where
        nCoord = nextCoordinate c nDir
        nDir = nextDir c dir

-- part 2
adjacents :: Coordinate -> [Coordinate]
adjacents (x, y) =
  [ (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1) -- adjacents
  , (x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1), (x + 1, y + 1) -- diagonals
  ]

nextVal :: M.Map (Int, Int) Int -> Int -> Coordinate -> Int
nextVal acc currentVal cord =
  foldr lookupAndSum currentVal $ adjacents cord where
    lookupAndSum a b = b + (fromMaybe 0 $ M.lookup a acc)

nextSum :: Int -> Int
nextSum 1 = 2
nextSum num = traverseGrid 2 ((1, 1), Up) $ M.fromList [((0, 0), 1), ((1, 0), 1)] where
  traverseGrid i (cord, dir) acc
    | i > num = i
    | otherwise = traverseGrid nVal (nCoord, nDir) (M.insert cord i acc) where
        nDir = nextDir cord dir
        nCoord = nextCoordinate cord nDir
        nVal = nextVal acc i nCoord
