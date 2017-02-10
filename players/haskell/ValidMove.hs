module ValidMove where

import Control.Monad

import qualified Action as A
import qualified Position as P
import qualified Direction as D
import qualified Weapon as W

-- Position (x,y) に居る侍が Action によって移動する場所を返す函数
-- 途中枠外に出てしまう場合は Nothing を返す
-- 警告：この函数だと居館上も移動できるので改善が必要 2/9
moveMaybe :: P.Position -> A.Action -> Maybe P.Position
moveMaybe (x,y) (A.Move D.South)
  | y+1 <= 14 = Just (x,y+1)
  | otherwise = Nothing
moveMaybe (x,y) (A.Move D.East)
  | x+1 <= 14 = Just (x+1,y)
  | otherwise = Nothing
moveMaybe (x,y) (A.Move D.North)
  | 0 <= y-1  = Just (x,y-1)
  | otherwise = Nothing
moveMaybe (x,y) (A.Move D.West)
  | 0 <= x-1  = Just (x-1,y)
  | otherwise = Nothing
moveMaybe (x,y) _ = Just (x,y)

validMove :: P.Position -> [A.Action] -> Bool
validMove (x,y) as
  | foldM moveMaybe (x,y) as /= Nothing = True
  | otherwise                           = False

-- return (0,0) >>= move (Move D.East) >>= move (Move D.South)
-- moveKnight :: (Int,Int) -> [(Int,Int)]
