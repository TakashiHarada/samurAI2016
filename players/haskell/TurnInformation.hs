-- $ hlint TurnInformation.hs

module TurnInformation where

import qualified SamuraiStates as SamuraiS
import qualified Data.Map as M
import qualified Army as A
import qualified Weapon as W
import qualified CurrentPosition as CP
import qualified OrderStatus as OS
import qualified ShowingStatus as ShS
import qualified TreatmentTurns as TT
import qualified BattlefieldState as BState
import qualified BattlefieldSection as BSection

---------- 1. Turn Number
type TurnNumber = Int

--Final :: TurnNumber
final = 96

isFinalTurn :: TurnNumber -> Bool
isFinalTurn x = x == final 

data GameData = GD {tnum :: TurnNumber,
                    sams :: SamuraiS.SamuraiStates,
                    bfs  :: BState.BattlefieldState} deriving (Show,Eq,Ord)

-------- Initial State

---- Turn Number
iniTurnNumber :: TurnNumber
iniTurnNumber = 0

-- BattlefieldState = M.Map CP.CurrentPosition BS.BattlefieldSection
iniBattleFieldState :: BState.BattlefieldState
iniBattleFieldState = foldl
  (\list (piece,point) -> M.update (\_ -> Just piece) point list)
  (M.fromList [((x,y), BSection.NotOccupied) | x <- [1..15], y <- [1..15]])
  [((BSection.Occupied A.Friend W.Spear), (0,0)),
   ((BSection.Occupied A.Friend W.Swords), (0,7)),
   ((BSection.Occupied A.Friend W.Axe), (7,0)),
   ((BSection.Occupied A.Enemy W.Spear), (14,14)),
   ((BSection.Occupied A.Enemy W.Swords), (14,7)),
   ((BSection.Occupied A.Enemy W.Axe), (7,14))
   ]

iniData :: GameData
iniData = GD iniTurnNumber iniSamuraiStates iniBattleFieldState

iniSamuraiStates :: SamuraiS.SamuraiStates
iniSamuraiStates =
  M.fromList
  [((A.Friend, W.Spear),SamuraiS.SamuraiState (0,0) OS.Yet ShS.Show 0),
   ((A.Friend, W.Swords),SamuraiS.SamuraiState (0,7) OS.Yet ShS.Show 0),
   ((A.Friend, W.Axe),SamuraiS.SamuraiState (7,0) OS.Yet ShS.Show 0),
   ((A.Enemy, W.Spear),SamuraiS.SamuraiState (14,14) OS.Yet ShS.Show 0),
   ((A.Enemy, W.Swords),SamuraiS.SamuraiState (14,7) OS.Yet ShS.Show 0),
   ((A.Enemy, W.Axe),SamuraiS.SamuraiState (7,14) OS.Yet ShS.Show 0)
   ]
