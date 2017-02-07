module DangerousPosition where

import Data.List
import Control.Applicative

import qualified GameData as GD
import qualified Army as A
import qualified GameInformation as GI
import qualified Position as P
import qualified HomePosition as HP
import qualified Weapon as W
import qualified AttackableArea as AA
import qualified SamuraiStates as SS
import qualified CanActionSamurai as CAS
import qualified GetShowingEnemy as GS

-- 区画 p は武器 w の味方が次の相手のターンで敵に攻撃され得る区画か否かを判定する函数
-- p が攻撃され得る区画ならば True，安全ならば False
-- 複数の GameData から危険かどうか判定できるように改善が必要
isDangerousPosition :: P.Position -> W.Weapon -> [GD.GameData] -> GI.GameInformation -> Maybe Bool
isDangerousPosition p w ((GD.GameData tn ss bs):gds) gi
  | tn == 95                              = Just False -- 最終ターンなのでどこもかしこも安全
  | p == HP.getHomePosition A.Friend w gi = Just False -- 居館は安全
  | otherwise
             = if length cse == 3
               then
                 if or $ map (p `elem`) eAttackaableArea
                 then Just True
                 else Just False
               else Nothing -- 確実には判定できない？
  where
    ce = CAS.canActionEnemy ss -- 行動可能な敵のリスト
    se = GS.getShowingEnemy ss -- 顕現している敵のリスト
    cse = intersect ce se      -- 行動可能で顕現している敵のリスト
    epos = map (\x -> SS.getSamuraiPosition x ss) cse -- 顕現している敵の位置のリスト
    eAttackaableArea = zipWith AA.getAttackableArea (map snd ce) epos

-- 次の相手のターンで敵が攻撃可能な区画の全てを返す函数
dangerousPositions :: [GD.GameData] -> [P.Position]
dangerousPositions = undefined

hoge :: GD.GameData
hoge = undefined
