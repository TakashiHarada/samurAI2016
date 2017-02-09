-- $ hlint hogePlayer.hs

--import Control.Monad
import System.IO
--import qualified TurnInformation as T
import qualified TurnNumber as TN
import qualified Ordering as O
import qualified GameInformation as GI
import qualified GameData as GD
import qualified Weapon as W
import qualified Action as A
import qualified Army as Army
import qualified Direction as D
import qualified Position as P
import qualified HomePosition as HP
       
main :: IO ()
main = do
  gameInfo <- GI.readGameInformation
  GI.respondToTheGameInformation 
  mainLoop [] gameInfo (HP.getInitHomePosition gameInfo)
  return ()

mainLoop :: [GD.GameData] -> GI.GameInformation -> P.EnemyPosition -> IO()
mainLoop gds gi epos = do
  gd <- fmap GD.divideComponent $ sequence $ take 22 $ repeat getLine
  --  O.sendOrderString $ O.reverseOrder gi $ detNextOrder (gd:gds) gi
  O.sendOrderString $ detNextOrder (gd:gds) gi
  if (TN.isFinalTurn $ GD.getTurnNumber gd)
    then do
      hFlush stdout
      return ()
    else do
      hFlush stdout
      mainLoop (gd:gds) gi epos

detNextOrder :: [GD.GameData] -> GI.GameInformation -> O.Order
detNextOrder (gd:gds) gi = case (GD.getTurnNumber gd) `mod` 3 of
  0 -> O.reverseOrder gi $ O.Order W.Spear  [A.Move D.East, A.Occupy D.South]
  1 -> O.reverseOrder gi $ O.Order W.Swords [A.Move D.East, A.Occupy D.South]
  2 -> O.reverseOrder gi $ O.Order W.Axe [A.Occupy D.West, A.Move D.East]
--detNextOrder gds = O.Order W.Spear [A.Occupy D.South, A.Move D.North]

{-
TODO::
 1. list-up available one order from current GameData with the following evaluation strategy.
    Evaluation Strategy
     e.g.
     * check existence of attackableEnemy. (if yes, return [Order])
     * size of occupation area
     * whether some Friend is in danger or not.
-}

inputExample :: [String]
inputExample =
  ["14",
   "0 6 1 0 0",
   "1 14 0 0 0",
   "9 12 0 0 0",
   "-1 -1 1 0 0",
   "-1 -1 0 0 0",
   "-1 -1 1 0 0",
   "0 9 9 9 9 9 9 2 9 9 9 9 9 9 9",
   "8 9 9 9 9 9 9 9 9 9 9 9 9 9 9",
   "8 8 9 9 9 9 9 9 9 9 9 9 9 9 9",
   "8 8 8 9 9 9 9 9 9 9 9 9 9 9 9",
   "8 8 8 8 9 9 9 9 9 9 9 9 9 9 9",
   "8 0 0 0 0 9 9 9 9 9 9 9 9 9 9",
   "0 8 8 8 8 8 9 9 9 9 9 9 9 9 9",
   "1 8 8 8 8 9 9 9 9 8 9 9 9 9 4",
   "0 8 8 8 9 9 9 9 8 8 3 9 9 9 9",
   "0 8 8 9 9 9 9 8 8 8 8 8 9 9 9",
   "8 8 8 9 9 9 8 8 8 8 8 8 8 9 9",
   "8 8 8 8 9 8 8 8 2 8 2 8 8 8 9",
   "1 1 8 8 8 8 8 8 2 2 2 8 8 8 8",
   "1 1 1 8 8 8 8 8 2 2 2 8 8 8 9",
   "8 1 1 1 8 8 8 5 2 2 2 8 8 9 3"]

{-
An example of Turn Information

14
0 6 1 0 0
1 14 0 0 0
9 12 0 0 0
-1 -1 1 0 0
-1 -1 0 0 0
-1 -1 1 0 0
0 9 9 9 9 9 9 2 9 9 9 9 9 9 9
8 9 9 9 9 9 9 9 9 9 9 9 9 9 9
8 8 9 9 9 9 9 9 9 9 9 9 9 9 9
8 8 8 9 9 9 9 9 9 9 9 9 9 9 9
8 8 8 8 9 9 9 9 9 9 9 9 9 9 9
8 0 0 0 0 9 9 9 9 9 9 9 9 9 9
0 8 8 8 8 8 9 9 9 9 9 9 9 9 9
1 8 8 8 8 9 9 9 9 8 9 9 9 9 4
0 8 8 8 9 9 9 9 8 8 3 9 9 9 9
0 8 8 9 9 9 9 8 8 8 8 8 9 9 9
8 8 8 9 9 9 8 8 8 8 8 8 8 9 9
8 8 8 8 9 8 8 8 2 8 2 8 8 8 9
1 1 8 8 8 8 8 8 2 2 2 8 8 8 8
1 1 1 8 8 8 8 8 2 2 2 8 8 8 9
8 1 1 1 8 8 8 5 2 2 2 8 8 9 3

--}
