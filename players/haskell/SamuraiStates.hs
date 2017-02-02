module SamuraiStates where

import qualified Data.Map as M
import qualified Army as A
import qualified Weapon as W
import qualified Position as CP
import qualified OrderStatus as OS
import qualified ShowingStatus as SS
import qualified TreatmentTurns as TT

type SamuraiStates = M.Map (A.Army,W.Weapon) SamuraiState

data SamuraiState
  = SamuraiState { getPosition :: CP.Position,
                   getOrderStatus :: OS.OrderStatus,
                   getShowingStatus :: SS.ShowingStatus,
                   getTreatmentTurns :: TT.TreatmentTurns }
  deriving (Show,Eq,Ord)

getSamuraiPosition :: (A.Army,W.Weapon) -> SamuraiStates -> CP.Position
getSamuraiPosition s hash = getPosition ss
  where ss = (\(Just x) -> x) (M.lookup s hash)
