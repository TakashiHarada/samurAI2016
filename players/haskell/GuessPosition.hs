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
-- 武器 w の敵が見えている(w == Sh.Show)場合は，見えている位置を返す
guessPostion :: W.Weapon -> GD.GameData -> P.Position -> P.Position
guessPostion W.Spear (GD.GameData _ ss _) _ | SS.getSamuraiShowingStatus sp ss == Sh.Show = SS.getSamuraiPosition sp ss where sp = (A.Enemy, W.Spear)
guessPostion W.Spear (GD.GameData _ ss bs) epos = undefined
guessPostion W.Swords (GD.GameData _ ss _) _ | SS.getSamuraiShowingStatus sw ss == Sh.Show = SS.getSamuraiPosition sw ss where sw = (A.Enemy, W.Swords)
guessPostion W.Swords (GD.GameData _ ss bs) epos = undefined
guessPostion W.Axe (GD.GameData _ ss _) _ | SS.getSamuraiShowingStatus axe ss == Sh.Show = SS.getSamuraiPosition axe ss where axe = (A.Enemy, W.Swords)
guessPostion W.Axe gd epos = undefined
