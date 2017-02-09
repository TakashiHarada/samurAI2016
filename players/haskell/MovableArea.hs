module MovableArea where

import Data.List

import qualified Army as A
import qualified Action as Action
import qualified Position as P
import qualified Weapon as W
import qualified GameInformation as GI
import qualified HomePosition as HP
import qualified Ordering as O

validMove :: O.Order -> GI.GameInformation -> P.Position -> Bool
validMove (O.Order w as) gi p = undefined
  where hp = HP.getHomePosition A.Friend w gi
        as' = filter Action.isMoveAction as
          
movableIn1 :: W.Weapon -> P.Position -> GI.GameInformation -> [P.Position]
movableIn1 w (x,y) gi =  (HP.getHomePosition A.Friend w gi) : ([(x+i,y+i) | i <- [-1,1], 0 <= (x+i), (x+i) <= 14, 0 <= (y+i), (y+i) <= 14] \\ prohibitedPosition)

movableIn2 :: W.Weapon -> P.Position -> GI.GameInformation -> [P.Position]
movableIn2 w (x,y) gi =  (HP.getHomePosition A.Friend w gi) : ([(x+i,y+i) | i <- [-2,2], 0 <= (x+i), (x+i) <= 14, 0 <= (y+i), (y+i) <= 14] \\ prohibitedPosition)

movableIn3 :: W.Weapon -> P.Position -> GI.GameInformation -> [P.Position]
movableIn3 w (x,y) gi =  (HP.getHomePosition A.Friend w gi) : ([(x+i,y+i) | i <- [-3,3], 0 <= (x+i), (x+i) <= 14, 0 <= (y+i), (y+i) <= 14] \\ prohibitedPosition)

prohibitedPosition :: [P.Position]
prohibitedPosition = [HP.friendSpearHome, HP.friendSwordsHome, HP.friendAxeHome, HP.enemySpearHome, HP.enemySwordsHome, HP.enemyAxeHome]
