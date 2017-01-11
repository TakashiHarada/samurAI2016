module TurnInformation where

import qualified Data.Map as M

---------- 1. Turn Number
type TurnNumber = Int

---------- 2. Samurai states
type CurrentPosition = (Int,Int)
data OrderStatus = Already | Yet
data ShowingStatus = Show | Hide
type TreatmentTurns = Int

data SamuraiState = SS { cp :: CurrentPosition,
                         os :: OrderStatus,
                         ss :: ShowingStatus,
                         tt :: TreatmentTurns }

---------- 3. Battlefield State

-- Battle field section
data BattlefieldSection =
    FriendSpear      -- 0
  | FriendSwords     -- 1
  | FriendBattleaxe  -- 2
  | EnemySpear       -- 3
  | EnemySwords      -- 4
  | EnemyBattleaxe   -- 5
  | NotOccupied      -- 8
  | NoInformation     -- 9

-- Battlefield State
type BattlefieldState = M.Map CurrentPosition BattlefieldSection
