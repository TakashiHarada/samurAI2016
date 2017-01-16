module Action where

import qualified Direction as D

data Action = Occupy D.Direction | Move D.Direction | Show | Hide
  deriving (Show,Eq,Ord)

actionCost :: Action -> Int
actionCost (Occupy _) = 4
actionCost (Move _) = 2
actionCost Show = 1
actionCost Hide = 1
