module Direction where

data Direction = South | East | North | West deriving (Show, Eq, Ord)

directionID :: Direction -> Int
directionID South = 1
directionID East  = 2
directionID North = 3
directionID West  = 4

reverse :: Direction -> Direction
reverse South = North
reverse East  = West
reverse North = South
reverse West  = East
