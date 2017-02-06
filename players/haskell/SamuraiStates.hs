module SamuraiStates where

import qualified Data.Map as M
import qualified Army as A
import qualified Weapon as W
import qualified Position as P
import qualified OrderStatus as OS
import qualified ShowingStatus as SS
import qualified TreatmentTurns as TT

type SamuraiStates = M.Map (A.Army,W.Weapon) SamuraiState

data SamuraiState
  = SamuraiState { getPosition :: P.Position,
                   getOrderStatus :: OS.OrderStatus,
                   getShowingStatus :: SS.ShowingStatus,
                   getTreatmentTurns :: TT.TreatmentTurns }
  deriving (Show,Eq,Ord)

getSamuraiPosition :: (A.Army,W.Weapon) -> SamuraiStates -> P.Position
getSamuraiPosition s hash = getPosition ss
  where ss = (\(Just x) -> x) (M.lookup s hash)

getSamuraiOrderStatus :: (A.Army,W.Weapon) -> SamuraiStates -> OS.OrderStatus
getSamuraiOrderStatus s hash = getOrderStatus ss
  where ss = (\(Just x) -> x) (M.lookup s hash)

getSamuraiShowingStatus :: (A.Army,W.Weapon) -> SamuraiStates -> SS.ShowingStatus
getSamuraiShowingStatus s hash = getShowingStatus ss
  where ss = (\(Just x) -> x) (M.lookup s hash)

getSamuraiTreatmentTurns :: (A.Army,W.Weapon) -> SamuraiStates -> TT.TreatmentTurns
getSamuraiTreatmentTurns s hash = getTreatmentTurns ss
  where ss = (\(Just x) -> x) (M.lookup s hash)

friendSpearInitSamuraiState :: SamuraiState
friendSpearInitSamuraiState = SamuraiState (0,0) OS.Yet SS.Show 0

friendSwordsInitSamuraiState :: SamuraiState
friendSwordsInitSamuraiState = SamuraiState (0,7) OS.Yet SS.Show 0

friendAxeInitSamuraiState :: SamuraiState
friendAxeInitSamuraiState = SamuraiState (7,0) OS.Yet SS.Show 0

enemySpearInitSamuraiState :: SamuraiState
enemySpearInitSamuraiState = SamuraiState (14,14) OS.Yet SS.Show 0

enemySwordsInitSamuraiState :: SamuraiState
enemySwordsInitSamuraiState = SamuraiState (14,7) OS.Yet SS.Show 0

enemyAxeInitSamuraiState :: SamuraiState
enemyAxeInitSamuraiState = SamuraiState (7,14) OS.Yet SS.Show 0

initSamuraiStates :: SamuraiStates
initSamuraiStates = M.fromList
  [((A.Friend, W.Spear), friendSpearInitSamuraiState),
   ((A.Friend, W.Swords), friendSwordsInitSamuraiState),
   ((A.Friend, W.Axe), friendAxeInitSamuraiState),
   ((A.Enemy, W.Spear), enemySpearInitSamuraiState),
   ((A.Enemy, W.Swords), enemySwordsInitSamuraiState),
   ((A.Enemy, W.Axe), enemyAxeInitSamuraiState)
  ]

-- getSamuraiOrderStatus (A.Enemy, W.Spear) initSamuraiStates
