module DangerousPosition where

import Data.List
import Control.Applicative
import Control.Monad

import qualified GameData as GD
import qualified Army as A
import qualified Action as Action
import qualified GameInformation as GI
import qualified Position as P
import qualified HomePosition as HP
import qualified Weapon as W
import qualified AttackableArea as AA
import qualified SamuraiStates as SS
import qualified CanActionSamurai as CAS
import qualified GetShowingEnemy as GS
import qualified Ordering as O

-- 区画 p は武器 w の味方が次の相手のターンで敵に攻撃され得る区画か否かを判定する函数
-- p が攻撃され得る区画ならば True，安全ならば False
isDangerousPosition :: P.Position -> W.Weapon -> SS.SamuraiStates -> P.EnemyPosition -> GI.GameInformation -> Bool
isDangerousPosition p w ss epos gi
  | p == HP.getHomePosition A.Friend w gi = False -- 居館は安全
  | or $ map (p `elem`) eAttackaableArea  = True
  | otherwise                             = False
  where
    ce = CAS.canActionEnemy ss -- 行動可能な敵のリスト
    eAttackaableArea = zipWith AA.getAttackableArea (map snd ce) ((\(x,y,z) -> [x,y,z]) epos)

-- 命令 o によって，攻撃を受ける可能性のある侍がいるかを判定する函数
-- O.move 未実装 2/9
willBeAttacked :: GD.GameData -> O.Order -> P.EnemyPosition -> GI.GameInformation -> Bool
willBeAttacked (GD.GameData tn _ _) (O.Order w as) _ _ | tn == 95 = False -- 最終ターンの次ターンは存在しない
willBeAttacked (GD.GameData _ ss bs) (O.Order w as) epos gi = or [oneIsDanger,twoIsDanger,actorIsDanger]
  where nws = delete w [W.Spear, W.Swords, W.Axe] -- 動作しない侍のリスト
        wpos = SS.getSamuraiPosition (A.Friend,w) ss -- 動作する侍 A のPosition
        one = SS.getSamuraiPosition (A.Friend,head nws) ss -- 動作しない侍 A のPosition
        two = SS.getSamuraiPosition (A.Friend,last nws) ss -- 動作しない侍 B のPosition
        oneIsDanger = isDangerousPosition one (head nws) ss epos gi -- 動作しない侍 A が攻撃されうるならば True
        twoIsDanger = isDangerousPosition two (last nws) ss epos gi -- 動作しない侍 B が攻撃されうるならば True
        actorIsDanger = isDangerousPosition (O.move (O.Order w as) wpos) w ss epos gi -- 動作しない侍 B が攻撃されうるならば True


-- 次の相手のターンで敵が攻撃可能な区画の全てを返す函数
dangerousPositions :: [GD.GameData] -> [P.Position]
dangerousPositions = undefined

-- -- 区画 p は武器 w の味方が次の相手のターンで敵に攻撃され得る区画か否かを判定する函数
-- -- p が攻撃され得る区画ならば True，安全ならば False
-- -- 複数の GameData から危険かどうか判定できるように改善が必要
-- isDangerousPosition :: P.Position -> W.Weapon -> [GD.GameData] -> GI.GameInformation -> Maybe Bool
-- isDangerousPosition p w ((GD.GameData tn ss bs):gds) gi
--   | tn == 95                              = Just False -- 最終ターンなのでどこもかしこも安全
--   | p == HP.getHomePosition A.Friend w gi = Just False -- 居館は安全
--   | otherwise
--              = if length cse == 3
--                then
--                  if or $ map (p `elem`) eAttackaableArea
--                  then Just True
--                  else Just False
--                else Nothing -- 確実には判定できない？
--   where
--     ce = CAS.canActionEnemy ss -- 行動可能な敵のリスト
--     se = GS.getShowingEnemy ss -- 顕現している敵のリスト
--     cse = intersect ce se      -- 行動可能で顕現している敵のリスト
--     epos = map (\x -> SS.getSamuraiPosition x ss) cse -- 顕現している敵の位置のリスト
--     eAttackaableArea = zipWith AA.getAttackableArea (map snd ce) epos
