module DangerousPosition where

import GameData as GD
import Position as P
import AttackableArea as AA

-- 区画 p が次の相手のターンで敵に攻撃され得る区画か否かを判定する函数
isDangerousPosition :: P.Position -> GD.GameData -> Bool
isDangerousPosition p gd = undefined

-- 次の相手のターンで攻撃可能な区画の全てを返す函数
dangerousPositions :: [GD.GameData] -> [P.Position]
dangerousPositions = undefined
