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
--import qualified GetShowingEnemy as GS
import qualified BattlefieldState as B
import qualified OrderStatus as OS

-- 敵の位置を推測する函数
-- 敵が見えている(w == Sh.Show)場合は,見えている位置を返す
guessEnemyPositions :: GI.GameInformation ->
                       [GD.GameData] -> P.EnemyPosition -> P.EnemyPosition
guessEnemyPositions _ [] (sp,ax,sw) = (sp,ax,sw)
guessEnemyPositions gi ((GD.GameData _ ss bs):gds) (sp,ax,sw) = (sp',ax',sw')
  where
    isShow :: W.Weapon -> Bool
    isShow w = SS.getSamuraiShowingStatus (A.Enemy, w) ss == Sh.Show
    isStay :: W.Weapon -> Bool
    isStay w = SS.getSamuraiOrderStatus (A.Enemy, w) ss == OS.Yet

    diff = B.getNewEnemyArea (GD.getBattlefieldState (head gds)) bs 
    sp' = if isStay W.Spear then sp
          else if isShow W.Spear
                 then SS.getSamuraiPosition (A.Enemy, W.Spear) ss
                 else guessPositionFromLog gi W.Spear diff sp
    ax' = if isStay W.Axe then ax
          else if isShow W.Axe
                 then SS.getSamuraiPosition (A.Enemy, W.Axe) ss
                 else guessPositionFromLog gi W.Axe diff sp
    sw' = if isStay W.Swords then sw
          else if isShow W.Swords
                 then SS.getSamuraiPosition (A.Enemy, W.Swords) ss
                 else guessPositionFromLog gi W.Swords diff sp

type Area = Int
-- if not Sh.Show. guess.
-- if Area > 0, enemy occupied some area in last turn, so he moved only 1 at most.
guessPositionFromLog :: GI.GameInformation -> W.Weapon ->
                        Area -> P.Position -> P.Position
-- I'm First = Enemy is Second = Enemy is likely to move North and West.
guessPositionFromLog GI.First  _ i (x,y) = if i > 0 then (x-1,y) else (x-1,y-1)
-- I'm Second = Enemy is First = Enemy is likely to move South and East.
guessPositionFromLog GI.Second _ i (x,y) = if i > 0 then (x+1,y) else (x+1,y+1)


-- FYI ------------------------
-- x+1 = East,  x-1 = West,
-- y+1 = South, y-1 = North
-------------------------------

--getMovablePositions :: P.Position -> [P.Position]
--getMovablePositions (x,y) = P.removeOutOfBoard [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]
-- E, S, W, N
