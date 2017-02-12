module GenerateOrder where

import qualified Action as A
import qualified Direction as D
import qualified Weapon as W
-- import qualified GameData as G
import qualified Ordering as O
-- import qualified MovableArea as M
-- import qualified DangerousPosition as DP
-- import qualified ValidMove as VM
-- import Data.Char

-- 動いてから占領するか，占領してから動く命令
oms :: [W.Weapon] -> [O.Order]
oms ws = [ O.Order w (as1++as2) |
        w <- ws,
        os <- [A.Occupy d | d <- [D.South, D.East, D.North, D.West]],
        ms <- [A.Move   d | d <- [D.South, D.East, D.North, D.West]],
        as1 <- [[os,ms]],
        as2 <- [[ms,os]]]

-- os :: [W.Weapon] -> [O.Order]
-- os ws = [O.Order w [os] |
--       w <- ws,
--       os <- [A.Occupy d | d <- [D.South, D.East, D.North, D.West]]]

-- ms1 :: [W.Weapon] -> [O.Order]
-- ms1 ws = [O.Order w [ms] | w <- ws, ms <- [A.Move   d | d <- [D.South, D.East, D.North, D.West]]]

-- ms2 :: [W.Weapon] -> [O.Order]
-- ms2 ws = [O.Order w mms |
--          w <- ws,
--          ms1 <- [A.Move   d | d <- [D.South, D.East, D.North, D.West]],
--          ms2 <- [A.Move   d | d <- [D.South, D.East, D.North, D.West]],
--          mms <- [[ms1,ms2]]]

-- ms3 :: [W.Weapon] -> [O.Order]
-- ms3 ws = [O.Order w mms |
--          w <- ws,
--          ms1 <- [A.Move   d | d <- [D.South, D.East, D.North, D.West]],
--          ms2 <- [A.Move   d | d <- [D.South, D.East, D.North, D.West]],
--          ms3 <- [A.Move   d | d <- [D.South, D.East, D.North, D.West]],
--          mms <- [[ms1,ms2,ms3]]]
