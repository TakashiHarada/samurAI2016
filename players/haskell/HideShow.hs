module HideShow where

import qualified Ordering as O
import qualified Action as A
import qualified Army as Army
-- import qualified GameData as G
import qualified SamuraiStates as SA
import qualified ShowingStatus as SS

addHideOrShow :: SA.SamuraiStates -> O.Order -> O.Order
addHideOrShow ss (O.Order w as)
  | showingstatus == SS.Hide = (O.Order w (A.Show:as))
  | otherwise                = (O.Order w (as++[A.Hide]))
  where
    showingstatus = SA.getSamuraiShowingStatus (Army.Friend,w) ss
