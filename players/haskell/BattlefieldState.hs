module BattlefieldState where

import qualified Data.Map as M
import qualified Position as P
import qualified BattlefieldSection as BS
import qualified Army as A
import qualified Weapon as W

type BattlefieldState = M.Map P.Position BS.BattlefieldSection

getNewEnemyArea :: BattlefieldState -> BattlefieldState -> Int
getNewEnemyArea old new = newEnemyOcpArea - oldEnemyOcpArea
  where
    oldEnemyOcpArea = length $ filter isEnemy $ map snd $ M.toList old
    newEnemyOcpArea = length $ filter isEnemy $ map snd $ M.toList new
    isEnemy = \bs -> bs == BS.Occupied A.Enemy W.Spear ||
                     bs == BS.Occupied A.Enemy W.Swords ||
                     bs == BS.Occupied A.Enemy W.Axe

