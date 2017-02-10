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
import qualified ShowingStatus as Sh
import qualified SamuraiStates as SS
import qualified CanActionSamurai as CAS
import qualified GetShowingEnemy as GS

-- 武器 w の敵の位置を推測する函数
-- 武器 w の敵が見えている(w == Sh.Show)場合は,見えている位置を返す
guessPosition :: W.Weapon -> [GD.GameData] -> P.Position -> P.Position
guessPosition w ((GD.GameData tn ss bs):gds) p
  = if (SS.getSamuraiShowingStatus (A.Enemy, w) ss == Sh.Show)
       then SS.getSamuraiPosition (A.Enemy, w) ss
       else guessPositionFromLog w ((GD.GameData tn ss bs):gds) p

-- if not Sh.Show. guess.
guessPositionFromLog :: W.Weapon -> [GD.GameData] -> P.Position -> P.Position                                                
guessPositionFromLog w gds p = head (getMovablePositions p)
-- FIXME:: 'head' should be changed!

getMovablePositions :: P.Position -> [P.Position]
getMovablePositions (x,y) = P.removeOutOfBoard [(x,y+1),(x,y-1),(x+1,y),(x-1,y)]
