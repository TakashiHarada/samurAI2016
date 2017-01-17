module BattlefieldState where

import Data.Map as M
import qualified CurrentPosition as CP
import qualified BattlefieldSection as BS

type BattlefieldState = M.Map CP.CurrentPosition BS.BattlefieldSection
