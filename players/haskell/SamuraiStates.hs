module SamuraiStates where

import qualified Data.Map as M
import qualified Army as A
import qualified Weapon as W
import qualified CurrentPosition as CP
import qualified OrderStatus as OS
import qualified ShowingStatus as SS
import qualified TreatmentTurns as TT

type SamuraiStates = M.Map (A.Army,W.Weapon) SamuraiState

data SamuraiState
  = SamuraiState { getCurrentPosition :: CP.CurrentPosition,
                   getOrderStatus :: OS.OrderStatus,
                   getShowingStatus :: SS.ShowingStatus,
                   getTreatmentTurns :: TT.TreatmentTurns }
  deriving (Show,Eq,Ord)
