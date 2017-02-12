module SearchAttackAdvantage where

import qualified Data.Map as M
import qualified Weapon as W
import qualified Position as P
import qualified Ordering as O
import qualified BattlefieldState as BSt
import qualified BattlefieldSection as BSe
-- import qualified Direction as D
import qualified Army as Ar
-- import qualified Action as Ac
import AttackArea


searchAttackAdvantage :: BSt.BattlefieldState-> P.Position -> O.Order -> Int
searchAttackAdvantage bst pos ord = foldl (\acc x -> acc + getAdvantage x) 0 aa
         where
           aa = map ((flip M.lookup) bst) $ attackArea pos ord

getAdvantage :: Maybe BSe.BattlefieldSection -> Int
getAdvantage bse | bse < Just (BSe.Occupied Ar.Friend W.Spear)  = 2
                 | bse == Just BSe.NotOccupied = 1
                 | otherwise = 0

-- hoge :: M.Map (Int,Int) BSe.BattlefieldSection
-- hoge = M.fromList [((0,0),(BSe.Occupied Ar.Friend W.Spear)),((1,0),(BSe.Occupied Ar.Friend W.Spear)),((2,0),(BSe.Occupied Ar.Enemy W.Axe)),((3,0),(BSe.Occupied Ar.Enemy W.Swords)),((4,0),BSe.NotOccupied),((0,1),BSe.NotOccupied),((1,1),BSe.NotOccupied),((2,1),BSe.NotOccupied),((3,1),BSe.NotOccupied),((4,1),BSe.NotOccupied),((0,2),(BSe.Occupied Ar.Friend W.Spear)),((1,2),BSe.NotOccupied),((2,2),BSe.NotOccupied),((3,2),BSe.NotOccupied),((4,2),BSe.NotOccupied),((0,3),(BSe.Occupied Ar.Friend W.Spear)),((1,3),BSe.NotOccupied),((2,3),BSe.NotOccupied),((3,3),BSe.NotOccupied),((4,3),BSe.NotOccupied),((0,4),(BSe.Occupied Ar.Friend W.Spear)),((1,4),BSe.NotOccupied),((2,4),BSe.NotOccupied),((3,4),BSe.NotOccupied),((4,4),BSe.NotOccupied)]
