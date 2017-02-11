module GuessPosition where

--import Data.List
--import Control.Applicative

import qualified GameData as GD
import qualified Army as A
import qualified GameInformation as GI
import qualified Position as P
--import qualified HomePosition as HP
import qualified Weapon as W
--import qualified AttackableArea as AA
import qualified ShowingStatus as Sh
import qualified SamuraiStates as SS
--import qualified CanActionSamurai as CAS
import qualified GetShowingEnemy as GS
import qualified BattlefieldState as B

-- 敵の位置を推測する函数
-- 敵が見えている(w == Sh.Show)場合は,見えている位置を返す
guessEnemyPositions :: GI.GameInformation ->
                       [GD.GameData] -> P.EnemyPosition -> P.EnemyPosition
guessEnemyPositions _ [] (sp,ax,sw) = (sp,ax,sw)
guessEnemyPositions gi ((GD.GameData tn ss bs):gds) (sp,ax,sw) = (sp',ax',sw')
  where
    isSpearShow  = SS.getSamuraiShowingStatus (A.Enemy, W.Spear)  ss == Sh.Show
    isAxeShow    = SS.getSamuraiShowingStatus (A.Enemy, W.Axe)    ss == Sh.Show
    isSwordsShow = SS.getSamuraiShowingStatus (A.Enemy, W.Swords) ss == Sh.Show
    --isSpearStay  = 
    diff = B.diffBattlefieldState (GD.getBattlefieldState (head gds)) bs 
    sp' = if isSpearShow  then SS.getSamuraiPosition (A.Enemy, W.Spear) ss
                          else guessPositionFromLog gi W.Spear diff sp
    ax' = if isAxeShow    then SS.getSamuraiPosition (A.Enemy, W.Axe) ss
                          else guessPositionFromLog gi W.Axe diff sp
    sw' = if isSwordsShow then SS.getSamuraiPosition (A.Enemy, W.Swords) ss
                          else guessPositionFromLog gi W.Swords diff sp
                              
-- if not Sh.Show. guess.
guessPositionFromLog :: GI.GameInformation -> W.Weapon ->
                        B.BattlefieldState -> P.Position -> P.Position
-- I'm First = Enemy is Second = Enemy is likely to move North and West.
guessPositionFromLog GI.First  w diff p = head (getMovablePositions p)
-- I'm Second = Enemy is First = Enemy is likely to move South and East.
guessPositionFromLog GI.Second w diff p = head (getMovablePositions p)

-- x+1 = East, x-1 = West, y+1 = South, y-1 = North
getMovablePositions :: P.Position -> [P.Position]
getMovablePositions (x,y) = P.removeOutOfBoard [(x,y+1),(x,y-1),(x+1,y),(x-1,y)]
