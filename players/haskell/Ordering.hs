module Ordering where

import qualified Action as A
import qualified Direction as D
import qualified Weapon as W
import qualified GameData as G
import qualified GameInformation as GI
import Data.Char
import System.IO

data Order = Order { getWeapon :: W.Weapon, getActions :: [A.Action] } deriving (Show,Eq,Ord)

-- e.g. : Order W.Spear [A.Occupy D.South]
--        Order W.Spear [A.Occupy D.South, A.Move D.North, A.Hide]
--        Order W.Spear [A.Occupy D.South, A.Move D.North, A.Show]              
                 
orderCostLimit :: Int
orderCostLimit = 7

sendOrderString :: Order -> IO()
sendOrderString (Order w as) = do
  putStrLn $ foldr (\x y -> x : ' ' : y) ['0'] $ (intToDigit $ W.weaponID w) : map (intToDigit . A.actionID) as
  hFlush stdout

orderCost :: Order -> Int
orderCost order = sum $ map A.actionCost (getActions order)

-- 命令 o によって占領する区画の数を得る函数
occupiedSections :: G.GameData -> Order -> Int
occupiedSections gd o = undefined

-- 命令 o によって敵から奪う区画の数を得る函数
takeSections :: G.GameData -> Order -> Int
takeSections = undefined

reverseOrder :: GI.GameInformation -> Order -> Order
reverseOrder GI.First  x = x
reverseOrder GI.Second (Order w as) = Order w (map A.reverse as)
