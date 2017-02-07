module GuessPosition where

import Data.List
import Control.Applicative

import qualified GameData as GD
import qualified Army as A
import qualified GameInformation as GI
import qualified Position as P
import qualified HomePosition as HP
import qualified Weapon as W
import qualified AttackableArea as AA
import qualified SamuraiStates as SS
import qualified CanActionSamurai as CAS
import qualified GetShowingEnemy as GS


guessPostion :: W.Weapon -> [GD.GameData] -> P.Position
guessPostion w (gd:gds) = undefined
