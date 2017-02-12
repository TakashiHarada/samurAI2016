module MoveToEffectivePosition where

import qualified Data.Map as M
import qualified Weapon as W
import qualified Position as P
import qualified Ordering as O
import qualified BattlefieldState as BSt
import qualified BattlefieldSection as BSe
-- import qualified Direction as D
import qualified Army as Ar
-- import qualified Action as Ac
import AttackArea

moveToEffectivePosition :: BSt.BattlefieldState-> P.Position -> O.Order
moveToEffectivePosition = bst (x,y) | candidate = undefined
                                    | candidate = undefined
                                    | candidate = undefined
                                    | candidate = undefined 
    where
      candidate = bst (x,y) = map (foldl (\acc x -> acc + getAdvantage x) 0) $ map (map ((flip M.lookup) bst)) rdrululd
      rdrululd = [
        [(a,b) | (a,b) <- [(x,y+1),(x,y+2),(x,y+3),(x,y+4),(x,y+5),(x+1,y),(x+1,y+1),(x+1,y+2),(x+1,y+3),(x+1,y+4),(x+2,y),(x+2,y+1),(x+2,y+2),(x+2,y+3),(x+3,y),(x+3,y+1),(x+3,y+2),(x+4,y),(x+4,y+1),(x+5,y)],0 <= a && a <= 14,0 <= b && b <= 14],
        [(a,b) | (a,b) <- [(x,y-1),(x,y-2),(x,y-3),(x,y-4),(x,y-5),(x+1,y),(x+1,y-1),(x+1,y-2),(x+1,y-3),(x+1,y-4),(x+2,y),(x+2,y-1),(x+2,y-2),(x+2,y-3),(x+3,y),(x+3,y-1),(x+3,y-2),(x+4,y),(x+4,y-1),(x+5,y)],0 <= a && a <= 14,0 <= b && b <= 14],
        [(a,b) | (a,b) <- [(x,y-1),(x,y-2),(x,y-3),(x,y-4),(x,y-5),(x-1,y),(x-1,y-1),(x-1,y-2),(x-1,y-3),(x-1,y-4),(x-2,y),(x-2,y-1),(x-2,y-2),(x-2,y-3),(x-3,y),(x-3,y-1),(x-3,y-2),(x-4,y),(x-4,y-1),(x-5,y)],0 <= a && a <= 14,0 <= b && b <= 14],
        [(a,b) | (a,b) <- [(x,y+1),(x,y+2),(x,y+3),(x,y+4),(x,y+5),(x-1,y),(x-1,y+1),(x-1,y+2),(x-1,y+3),(x-1,y+4),(x-2,y),(x-2,y+1),(x-2,y+2),(x-2,y+3),(x-3,y),(x-3,y+1),(x-3,y+2),(x-4,y),(x-4,y+1),(x-5,y)],0 <= a && a <= 14,0 <= b && b <= 14]
        ]
