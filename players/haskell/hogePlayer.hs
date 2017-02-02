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
import qualified Position as P
       
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
detNextOrder (gd:gds) = case (GD.getTurnNumber gd) `mod` 3 of
  0 -> O.Order W.Spear  [A.Move D.East, A.Occupy D.South]
  1 -> O.Order W.Swords [A.Move D.South, A.Occupy D.South]
  2 -> O.Order W.Axe [A.Occupy D.South, A.Move D.East]
--detNextOrder gds = O.Order W.Spear [A.Occupy D.South, A.Move D.North]

hogeTurn :: Int -> O.Order
hogeTurn 1 = O.Order W.Spear [A.Occupy D.East, A.Move D.East]
hogeTurn 3 = O.Order W.Axe [A.Move D.South, A.Move D.West, A.Move, D.West]
hogeTurn 5 = O.Order W.Swords [A.Move D.East, A.Move D.East, A.Move East]
hogeTurn _ = undefined

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

getPosition :: [GD.GameData] -> W.Weapon -> Maybe P.Position
getPosition = undefined

isDangerousPlace :: P.Position -> GD.GameData -> Bool
isDangerousPlace = undefined

dangerousPlaces :: [GD.GameData] -> [P.Position]
dangerousPlaces = undefined

canAttack :: W.Weapon -> G.GameData -> Bool
canAttack = undefined

canAttackIn2 :: W.Weapon -> G.GameData -> Bool
canAttackIn2 = undefined

canAttackIn3 :: W.Weapon -> G.GameData -> Bool
canAttackIn3 = undefined

attackOrder :: W.Weapon -> G.GameData -> O.Order
attackOrder = undefined

occupyOrder :: W.Weapon -> G.GameData -> O.Order
occupyOrder = undefined



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
