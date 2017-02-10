module AttackArea where

import qualified Action as A
import qualified Direction as D
import qualified Weapon as W
import qualified Position as P
import qualified GameData as G
import qualified Ordering as O
import Data.Char

attackArea :: P.Position -> O.Order -> [P.Position]
attackArea _ (O.Order _ []) = []
--attackArea (x,y) _ | not(0 <= x && x <= 14) || not(0 <= y && y <= 14) = []
attackArea (x,y) (O.Order W.Spear (f:b))
         | f == A.Occupy D.South = [ (a,b) | (a,b) <- [(x,y+1),(x,y+2),(x,y+3),(x,y+4)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | f == A.Occupy D.East  = [ (a,b) | (a,b) <- [(x+1,y),(x+2,y),(x+3,y),(x+4,y)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | f == A.Occupy D.North = [ (a,b) | (a,b) <- [(x,y-1),(x,y-2),(x,y-3),(x,y-4)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | f == A.Occupy D.West  = [ (a,b) | (a,b) <- [(x-1,y),(x-2,y),(x-3,y),(x-4,y)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | not $ f >= A.Show = attackArea (moving (x,y) f) (O.Order W.Spear b)
         | otherwise = attackArea (x,y) (O.Order W.Spear b)
         where    
            home = [(0,0),(0,7),(7,0),(14,14),(14,7),(7,14)]         

attackArea (x,y) (O.Order W.Swords (f:b))
         | f == A.Occupy D.South = [ (a,b) | (a,b) <- [(x,y+2),(x,y+1),(x+1,y+1),(x+1,y),(x+2,y)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | f == A.Occupy D.East  = [ (a,b) | (a,b) <- [(x+2,y),(x+1,y),(x+1,y-1),(x,y-1),(x,y-2)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | f == A.Occupy D.North = [ (a,b) | (a,b) <- [(x,y-2),(x,y-1),(x-1,y-1),(x-1,y),(x-2,y)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | f == A.Occupy D.West  = [ (a,b) | (a,b) <- [(x-2,y),(x-1,y),(x-1,y+1),(x,y+1),(x,y+1)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | not $ f >= A.Show = attackArea (moving (x,y) f) (O.Order W.Swords b)
         | otherwise = attackArea (x,y) (O.Order W.Swords b)
         where    
            home = [(0,0),(0,7),(7,0),(14,14),(14,7),(7,14)]         

attackArea (x,y) (O.Order W.Axe (f:b))
         | f == A.Occupy D.South = [ (a,b) | (a,b) <- [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y+1),(x+1,y+1),(x+1,y),(x+1,y-1)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | f == A.Occupy D.East  = [ (a,b) | (a,b) <- [(x-1,y+1),(x,y+1),(x+1,y+1),(x+1,y),(x+1,y-1),(x,y-1),(x-1,y-1)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | f == A.Occupy D.North = [ (a,b) | (a,b) <- [(x+1,y+1),(x+1,y),(x+1,y-1),(x,y-1),(x-1,y-1),(x-1,y),(x-1,y+1)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | f == A.Occupy D.West  = [ (a,b) | (a,b) <- [(x+1,y-1),(x,y-1),(x-1,y-1),(x-1,y),(x-1,y+1),(x,y+1),(x+1,y+1)],0 <= a && a <= 14,0 <= b && b <= 14,not $ elem (a,b) home]
         | not $ f >= A.Show = attackArea (moving (x,y) f) (O.Order W.Axe b)
         | otherwise = attackArea (x,y) (O.Order W.Axe b)
         where    
            home = [(0,0),(0,7),(7,0),(14,14),(14,7),(7,14)]

moving :: P.Position -> A.Action -> P.Position
moving (x,y) (A.Move D.South) = (x,y+1)
moving (x,y) (A.Move D.East)  = (x+1,y)
moving (x,y) (A.Move D.North) = (x,y-1)
moving (x,y) (A.Move D.West)  = (x-1,y)
