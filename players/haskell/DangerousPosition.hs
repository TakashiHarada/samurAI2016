module DangerousPosition where

import GameData as GD
import Position as P
import AttackableArea as AA
import CanActionSamurai as CAS

-- 区画 p が次の相手のターンで敵に攻撃され得る区画か否かを判定する函数
-- p が攻撃され得る区画ならば True，安全ならば False
-- 複数の GameData から危険かどうか判定できるように改善が必要
isDangerousPosition :: P.Position -> [GD.GameData] -> Maybe Bool
isDangerousPosition _ ((GD.GameData 95 _ _):_) = Just False -- 最終ターンなのでどこもかしこも安全
isDangerousPosition (x,y) ((GD.GameData _ ss bs):gds) = undefined

-- 次の相手のターンで敵が攻撃可能な区画の全てを返す函数
dangerousPositions :: [GD.GameData] -> [P.Position]
dangerousPositions = undefined
