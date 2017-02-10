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

-- 自身の居館以外に侵入するような命令は来ないと仮定している．
--  --> ValidMove module の validMove を使う
move :: [Action] -> P.Position -> P.Position
move [] (x,y) = (x,y)
move ((Move D.South):as) (x,y) = move as (x,y+1)
move ((Move D.East):as)  (x,y) = move as (x+1,y)
move ((Move D.North):as) (x,y) = move as (x,y-1)
move ((Move D.West):as)  (x,y) = move as (x-1,y)
move (_:as) (x,y) = move as (x,y)
