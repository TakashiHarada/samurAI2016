module Ordering where

import qualified Action as A
import qualified Weapon as W
import qualified Direction as D
import System.IO

data Order = Order { weapon :: W.Weapon, actions :: [A.Action] }

-- e.g. : Order W.Spear [A.Occupy D.South]

sendOrderString :: [Order] -> IO()
sendOrderString orders = putStrLn "1 1 6 9 0" >>= \_ -> hFlush stdout --FIXME
