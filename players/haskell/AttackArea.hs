module AttackArea where

import qualified Action as A
import qualified Direction as D
import qualified Weapon as W
import qualified Position as P
import qualified GameData as G
import qualified Ordering as O
import Data.Char


attackArea :: P.Position -> O.Order -> [P.Position]
attackArea (x,y) (Order W.Spear (f:b))
         | f==A.Occupy D.South = [(x,y+1),(x,y+2),(x,y+3),(x,y+4)] 
           f==A.Occupy D.East  = [(x+1,y),(x+2,y),(x+3,y),(x+4,y)] 
           f==A.Occupy D.North = [(x,y-1),(x,y-2),(x,y-3),(x,y-4)] 
           f==A.Occupy D.West  = [(x-1,y),(x-2,y),(x-3,y),(x-4,y)]
           f==A.Move dir = moving dir 
                    
attackArea W.Swords (x,y) ord = undefined
attackArea W.Axe (x,y) ord = undefined


moving :: Position -> Order -> Position
moving (x,y) (Order _ (f:b)) = step
