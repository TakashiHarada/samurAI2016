module Action where

import qualified Direction as D
import qualified Position as P

data Action = Occupy D.Direction | Move D.Direction | Show | Hide
  deriving (Show,Eq,Ord)

actionCost :: Action -> Int
actionCost (Occupy _) = 4
actionCost (Move _) = 2
actionCost Show = 1
actionCost Hide = 1

actionID :: Action -> Int
actionID (Occupy d) = D.directionID d
actionID (Move d) = 4 + D.directionID d
actionID _ = 9

reverse :: Action -> Action
reverse (Occupy d) = Occupy (D.reverse d)
reverse (Move d)   = Move   (D.reverse d)
reverse a = a

isMoveAction :: Action -> Bool
isMoveAction (Move _) = True
isMoveAction _          = False

move :: Action -> P.Position -> Maybe P.Position
move (Move D.South) (x,y)
  | y+1 <= 14 = Just (x,y+1)
  | otherwise = Nothing
move (Move D.East) (x,y)
  | x+1 <= 14 = Just (x+1,y)
  | otherwise = Nothing
move (Move D.North) (x,y)
  | 0 <= y-1  = Just (x,y-1)
  | otherwise = Nothing
move (Move D.West) (x,y)
  | 0 <= x-1  = Just (x-1,y)
  | otherwise = Nothing
move _ (x,y)              = Just (x,y)

-- return (0,0) >>= move (Move D.East) >>= move (Move D.South)
