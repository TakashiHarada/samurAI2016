module GenerateOrder where

import qualified Action as A
import qualified Direction as D
import qualified Weapon as W
import qualified GameData as G
import qualified Ordering as O
import Data.Char

-- 動いてから占領するか，占領してから動く命令
oms :: [W.Weapon] -> [O.Order]
oms ws = [ O.Order w as |
        w <- ws,
        os <- [A.Occupy d | d <- [D.South, D.East, D.North, D.West]],
        ms <- [A.Move   d | d <- [D.South, D.East, D.North, D.West]],
        as <- [[os,ms]]]

os :: [W.Weapon] -> [O.Order]
os ws = [O.Order w [os] |
      w <- ws,
      os <- [A.Occupy d | d <- [D.South, D.East, D.North, D.West]]]
