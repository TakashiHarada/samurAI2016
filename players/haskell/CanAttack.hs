module CanAttack where

import Data.List

import qualified Army as A
import qualified Weapon as W
import qualified GameData as GD
import qualified Ordering as O
import qualified CanActionSamurai as CAS
import qualified GetShowingEnemy as GS
import qualified SamuraiStates as SS
import qualified AttackableArea as AA

-- 現在のターンで攻撃可能な敵がいるか
-- 複数のゲームデータから判断するように改善が必要 20160206
canAttack :: [GD.GameData] -> Maybe Bool
canAttack ((GD.GameData tn ss bs):gds)
  | null es = Nothing -- 敵が全員隠伏していたら確実な判断は出来ない
  | otherwise = if or $ fmap (elem) epos <*> attackableArea
                then Just True
                else Just False
  where es = GS.getShowingEnemy ss  -- 顕現している敵のリスト
        fs = CAS.canActionFriend ss -- 行動可能な味方のリスト
        epos = map (\x -> SS.getSamuraiPosition x ss) es -- 各敵の現在位置のリスト
        fpos = map (\x -> SS.getSamuraiPosition x ss) fs -- 各味方の現在位置のリスト
        attackableArea = zipWith AA.getAttackableArea (map snd fs) fpos


-- -- 現在，敵 w を攻撃可能か判定する函数
-- canAttack :: W.Weapon -> GD.GameData -> Maybe Bool
-- canAttack w gd = undefined

-- -- 都合良くいけば2ターンで，敵 w を攻撃可能か判定する函数
-- canAttackIn2 :: W.Weapon -> GD.GameData -> Maybe Bool
-- canAttackIn2 w gd = undefined

-- -- 都合良くいけば3ターンで，敵 w を攻撃可能か判定する函数
-- canAttackIn3 :: W.Weapon -> GD.GameData -> Maybe Bool
-- canAttackIn3 w gd = undefined
