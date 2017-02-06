module Action where

import qualified Direction as D

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
