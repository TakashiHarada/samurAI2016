module CanAttack where

import Weapon as W
import GameData as GD
import Ordering as O

-- 現在，敵 w を攻撃可能か判定する函数
canAttack :: W.Weapon -> GD.GameData -> Bool
canAttack w gd = undefined

-- 都合良くいけば2ターンで，敵 w を攻撃可能か判定する函数
canAttackIn2 :: W.Weapon -> GD.GameData -> Bool
canAttackIn2 w gd = undefined

-- 都合良くいけば3ターンで，敵 w を攻撃可能か判定する函数
canAttackIn3 :: W.Weapon -> GD.GameData -> Bool
canAttackIn3 w gd = undefined
