module Ordering where

import qualified Action as A
import qualified Weapon as W
import qualified Direction as D
import Data.Char
import System.IO

data Order = Order { getWeapon :: W.Weapon, getActions :: [A.Action] } deriving (Show,Eq,Ord)

-- e.g. : Order W.Spear [A.Occupy D.South]
--        Order W.Spear [A.Occupy D.South, A.Move D.North, A.Hide]

orderCostLimit = 7

sendOrderString :: Order -> IO()
sendOrderString (Order w as) = do
  putStrLn $ (intToDigit $ W.weaponID w) : map (intToDigit . A.actionID) as
  hFlush stdout

orderCost :: Order -> Int
orderCost order = sum $ map A.actionCost (getActions order)
