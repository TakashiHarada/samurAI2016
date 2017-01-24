module Tactic where

import qualified Ordering as O
import qualified Weapon as W
import qualified Action as A
import qualified Direction as D
import qualified GameData as G

-- If we attack the Enemy X then True else False, where X is a Weapon.
canAttackX :: W.Weapon -> G.GameData -> Bool
canAttackX w (G.GameData _ ss bs) = undefined

canAttackSomebody :: G.GameData -> Bool
canAttackSomebody (G.GameData _ ss bs) = undefined

{-
initial States

Weapon Firend Enemy
Spear  (0,0)  (14,14)
Sword  (0,7)  (14,7)
Axe    (7,0)  (7,14)
-}
