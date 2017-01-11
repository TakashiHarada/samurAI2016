-- $ hlint TurnInformation.hs

module TurnInformation where

import qualified Data.Map as M

---------- 1. Turn Number
type TurnNumber = Int

---------- 2. Samurai states
type CurrentPosition = (Int,Int)
<<<<<<< HEAD
data OrderStatus = Already | Yet
data ShowingStatus = Show | Hide
type TreatmentTurns = Int
=======
data OrderStatus = Already | Yet deriving (Show,Eq,Ord)
data ShowingStatus = Show | Hide deriving (Show,Eq,Ord)
type TreatmentTurn = Int
>>>>>>> c51b8810d6361547d3c788c1f32cadf62f3053dc

data SamuraiState = SS { cp :: CurrentPosition,
                         os :: OrderStatus,
                         ss :: ShowingStatus,
<<<<<<< HEAD
                         tt :: TreatmentTurns }
=======
                         tt :: TreatmentTurn } deriving (Show,Eq,Ord)
>>>>>>> c51b8810d6361547d3c788c1f32cadf62f3053dc

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
<<<<<<< HEAD
  | NoInformation     -- 9

=======
  | NoInromation     -- 9
  deriving (Show,Eq,Ord)
           
>>>>>>> c51b8810d6361547d3c788c1f32cadf62f3053dc
-- Battlefield State
type BattlefieldState = M.Map CurrentPosition BattlefieldSection

-------- Initial State
---- Turn Number
turnNumber :: TurnNumber
turnNumber = 0

---- Samurai States
fSpear :: SamuraiState
fSpear = SS (0,0) Yet Show 0

fSwords :: SamuraiState
fSwords = SS (0,7) Yet Show 0

fAxe :: SamuraiState
fAxe = SS (7,0) Yet Show 0

eSpear :: SamuraiState
eSpear = SS (14,14) Yet Show 0

eSwords :: SamuraiState
eSwords = SS (14,7) Yet Show 0

eAxe :: SamuraiState
eAxe = SS (7,14) Yet Show 0

data Samurai = Fspear | Fswords | Faxe | Espear | Eswords | Eaxe
  deriving (Show,Eq,Ord)

type SamuraiStates = M.Map Samurai SamuraiState

samuraiStates :: SamuraiStates
samuraiStates = M.fromList [(Fspear,fSpear),(Fswords,fSwords),(Faxe,fAxe),(Espear,eSpear),(Eswords,eSwords),(Eaxe,eAxe)]

-- Battlefield State
battleFieldState :: BattlefieldState
battleFieldState = foldl
  (\list (piece,point) -> M.update (\_ -> Just piece) point list)
  (M.fromList [((x,y), NotOccupied) | x <- [1..15], y <- [1..15]])
  [(FriendSpear, (0,0)), (FriendSwords, (0,7)), (FriendBattleaxe, (7,0)), (EnemySpear, (14,14)), (EnemySwords, (14,7)), (EnemyBattleaxe,(7,14))]
