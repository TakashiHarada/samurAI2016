module Ordering where

import qualified Action as A
import qualified Weapon as W
import qualified Direction as D
import System.IO

data Order = Order { weapon :: W.Weapon, actions :: [A.Action] }

-- e.g. : Order W.Spear [A.Occupy D.South]
--        Order W.Spear [A.Occupy D.South, A.Move D.North, A.Hide]

orderCostLimit = 7

sendOrderString :: [Order] -> IO()
sendOrderString orders = putStrLn "1 1 6 9 0" >>= \_ -> hFlush stdout --FIXME

orderCost :: Order -> Int
orderCost order = sum $ map A.actionCost (actions order)
