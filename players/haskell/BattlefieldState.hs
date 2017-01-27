module BattlefieldState where

import Data.Map as M
import qualified Position as CP
import qualified BattlefieldSection as BS

type BattlefieldState = M.Map CP.Position BS.BattlefieldSection
