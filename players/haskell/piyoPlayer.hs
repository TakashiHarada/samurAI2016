-- $ hlint hogePlayer.hs

import Control.Monad
import System.IO
import qualified TurnInformation as T
import qualified TurnNumber as TN
import qualified Ordering as O
import qualified GameInformation as GI
import qualified GameData as GD
import qualified Weapon as W
import qualified Action as A
import qualified Direction as D
       
main :: IO ()
main = do
  gameInfo <- GI.readGameInformation
  GI.respondToTheGameInformation 
  mainLoop []
  return ()

mainLoop :: [GD.GameData] -> IO()
mainLoop gds = do
  gd <- fmap GD.divideComponent $ sequence $ take 22 $ repeat getLine
  O.sendOrderString $ detNextOrder (gd:gds)
  if (TN.isFinalTurn $ GD.getTurnNumber gd)
    then do
      hFlush stdout
      return ()
    else do
      hFlush stdout
      mainLoop (gd:gds)

-- TODO:: Implement!
detNextOrder :: [GD.GameData] -> O.Order
detNextOrder (gd:gds) = case GD.getTurnNumber gd of
  1 -> turn1
  3 -> turn3
  5 -> turn5
  7 -> turn7
  9 -> turn9
  11 -> turn11  
  13 -> turn13
  15 -> turn15
  17 -> turn17
  x -> turn x
--detNextOrder gds = O.Order W.Spear [A.Occupy D.South, A.Move D.North]
--hogeTurn :: Int -> O.Order

turn1 = O.Order W.Spear [A.Occupy D.East, A.Move D.East]
turn3 = O.Order W.Axe [A.Move D.South, A.Move D.West, A.Move D.West]
turn5 = O.Order W.Swords [A.Move D.East, A.Move D.East, A.Move D.East]

turn7 = O.Order W.Spear [A.Move D.East, A.Move D.East, A.Move D.East]
turn9 = O.Order W.Axe [A.Move D.South, A.Occupy D.West]
turn11 = O.Order W.Swords [A.Move D.East, A.Move D.East, A.Move D.East]

turn13 = O.Order W.Spear [A.Move D.South, A.Occupy D.West]
turn15 = O.Order W.Axe [A.Move D.South, A.Occupy D.West]
turn17 = O.Order W.Swords [A.Move D.South, A.Occupy D.South]

turn x | x `mod` 6 == 1 = O.Order W.Spear [A.Move D.South, A.Occupy D.West]
       | x `mod` 6 == 3 = O.Order W.Axe [A.Move D.South, A.Occupy D.West]
       | x `mod` 6 == 5 = O.Order W.Swords [A.Move D.South, A.Occupy D.South]
       | otherwise = O.Order W.Spear [A.Move D.South, A.Occupy D.West]

hoge = ["14",
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

{--

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
