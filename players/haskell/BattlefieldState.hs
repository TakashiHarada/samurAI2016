module BattlefieldState where

import Data.Map as M
import qualified Position as P
import qualified BattlefieldSection as BS

type BattlefieldState = M.Map P.Position BS.BattlefieldSection

diffBattlefieldState :: BattlefieldState -> BattlefieldState -> BattlefieldState
diffBattlefieldState old new = new
