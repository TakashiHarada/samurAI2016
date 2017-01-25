module Tactic where

import Data.Map as M
import qualified Ordering as O
import qualified Weapon as W
import qualified Action as Ac
import qualified Army as Ar
import qualified Direction as D
import qualified GameData as G
import qualified CurrentPosition as C
import qualified SamuraiStates as SS

-- If we attack the Enemy X then True else False, where X is a Weapon.
canAttackX :: W.Weapon -> G.GameData -> Bool
canAttackX w (G.GameData _ ss bs) = undefined

canAttackSomebody :: G.GameData -> Bool
canAttackSomebody (G.GameData _ ss bs) = undefined

getAttackableAreaOfX :: W.Weapon -> C.CurrentPosition -> [C.CurrentPosition]
getAttackableAreaOfX W.Spear (x,y) = Prelude.filter (\(s,t) -> s <= 14 && t <= 14 && 0 <= s && 0 <= t)
  [(x+i,y+j) | i <- [-1..1], j <- [-4..4]] ++ [(x+i,y+j) | i <- [-4..4], j <- [-1..1]] ++ [(x-5,y),(x+5,y),(x,y-5),(x,y+5)]
getAttackableAreaOfX W.Swords (x,y) =
  Prelude.filter (\(s,t) -> s <= 14 && t <= 14 && 0 <= s && 0 <= t) [(x+i,y+j) | i <- [-3..3], j <- [-3..3], abs i + abs j <= 3]
getAttackableAreaOfX W.Axe (x,y) =
  Prelude.filter (\(s,t) -> s <= 14 && t <= 14 && 0 <= s && 0 <= t) [(x+i,y+j) | i <- [-2..2], j <- [-2..2]]

{-
initial States

Weapon Firend Enemy
Spear  (0,0)  (14,14)
Sword  (0,7)  (14,7)
Axe    (7,0)  (7,14)
-}
