module CanActionSamurai where

import qualified GameData as G
import qualified Weapon as W
import qualified Army as A
import qualified OrderStatus as OS
import qualified SamuraiStates as SS

-- data GameData = GameData { getTurnNumber :: T.TurnNumber,
--                            getSamuraiStates :: S.SamuraiStates,
--                            getBattlefieldState :: B.BattlefieldState }

-- 次のターンに攻撃可能な敵を列挙
canActionEnemy :: G.GameData -> [W.Weapon]
canActionEnemy (G.GameData 95 ss _) = [] -- 最終ターンなので相手に次のターンはない
canActionEnemy (G.GameData _ ss _)  = map snd $ filter (\x -> OS.Yet == SS.getSamuraiOrderStatus x ss) [(A.Enemy,W.Spear), (A.Enemy,W.Swords), (A.Enemy,W.Axe)]
