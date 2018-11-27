module Day3 where

import Prelude hiding (Right, Left)

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
    | otherwise = traverseMemory (i + 1) ((nextCoordinate c nDir), nDir) where
        nDir = (nextDir c dir)
