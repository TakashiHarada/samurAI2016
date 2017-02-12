module ValidMove where

import Control.Monad
import Data.List

import qualified Action as A
import qualified Army as Army
import qualified Position as P
import qualified Direction as D
import qualified Weapon as W
import qualified GameInformation as GI
import qualified HomePosition as HP

-- 与えられた命令は，移動できない区画に移動している命令であるかを判定する函数
validMove :: P.Position -> [A.Action] -> W.Weapon -> GI.GameInformation -> [P.Position] -> Bool
validMove (x,y) as w gi ps
  | foldM (\p a -> moveMaybe p w gi a ps) (x,y) as /= Nothing = True
  | otherwise                                              = False

-- ps (顕現している侍がいる区画) にも移動することはできないことに留意されたし．
moveMaybe :: P.Position -> W.Weapon -> GI.GameInformation -> A.Action -> [P.Position] -> Maybe P.Position
moveMaybe (x,y) w gi (A.Move D.South) ps
  | y+1 <= 14 && (not ((x,y+1) `elem` bad w)) = Just (x,y+1)
  | otherwise = Nothing
  where
    hp = \weapon -> HP.getHomePosition Army.Friend weapon gi
    pp = ps ++ [(0,0),(0,7),(7,0),(14,14),(14,7),(7,14)]
    bad = \weapon -> delete (hp weapon) pp
moveMaybe (x,y) w gi (A.Move D.East) ps
  | x+1 <= 14 && (not ((x+1,y) `elem` bad w)) = Just (x+1,y)
  | otherwise = Nothing
  where
    hp = \weapon -> HP.getHomePosition Army.Friend weapon gi
    pp = ps ++ [(0,0),(0,7),(7,0),(14,14),(14,7),(7,14)]
    bad = \weapon -> delete (hp weapon) pp
moveMaybe (x,y) w gi (A.Move D.North) ps
  | 0 <= y-1 && (not ((x,y-1) `elem` bad w)) = Just (x,y-1)
  | otherwise = Nothing
  where
    hp = \weapon -> HP.getHomePosition Army.Friend weapon gi
    pp = ps ++ [(0,0),(0,7),(7,0),(14,14),(14,7),(7,14)]
    bad = \weapon -> delete (hp weapon) pp
moveMaybe (x,y) w gi (A.Move D.West) ps
  | 0 <= x-1 && (not ((x-1,y) `elem` bad w)) = Just (x-1,y)
  | otherwise = Nothing
  where
    hp = \weapon -> HP.getHomePosition Army.Friend weapon gi
    pp = ps ++ [(0,0),(0,7),(7,0),(14,14),(14,7),(7,14)]
    bad = \weapon -> delete (hp weapon) pp
moveMaybe (x,y) _ _ _ _ = Just (x,y)

-- Position (x,y) に居る侍が Action によって移動する場所を返す函数
-- 途中枠外に出てしまう場合は Nothing を返す
-- 警告：この函数だと居館上も移動できるので改善が必要 2/9
-- moveMaybe' :: P.Position -> A.Action -> Maybe P.Position
-- moveMaybe' (x,y) (A.Move D.South)
--   | y+1 <= 14 = Just (x,y+1)
--   | otherwise = Nothing
-- moveMaybe' (x,y) (A.Move D.East)
--   | x+1 <= 14 = Just (x+1,y)
--   | otherwise = Nothing
-- moveMaybe' (x,y) (A.Move D.North)
--   | 0 <= y-1  = Just (x,y-1)
--   | otherwise = Nothing
-- moveMaybe' (x,y) (A.Move D.West)
--   | 0 <= x-1  = Just (x-1,y)
--   | otherwise = Nothing
-- moveMaybe' (x,y) _ = Just (x,y)

-- validMove' :: P.Position -> [A.Action] -> Bool
-- validMove' (x,y) as
--   | foldM moveMaybe' (x,y) as /= Nothing = True
--   | otherwise                           = False


-- return (0,0) >>= move (Move D.East) >>= move (Move D.South)
-- moveKnight :: (Int,Int) -> [(Int,Int)]
