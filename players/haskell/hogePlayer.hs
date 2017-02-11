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
import qualified ValidMove as VM
import qualified GenerateOrder as GO
import qualified CanActionSamurai as CAS
import qualified SamuraiStates as SS
import qualified AttackArea as AA
import qualified GuessPosition as GP
import qualified DangerousPosition as DP
import qualified HideShow as HS

main :: IO ()
main = do
  gameInfo <- GI.readGameInformation
  GI.respondToTheGameInformation 
  mainLoop [] gameInfo (HP.getInitHomePosition gameInfo)
  return ()

mainLoop :: [GD.GameData] -> GI.GameInformation -> P.EnemyPosition -> IO()
mainLoop gds gi epos = do
  gd <- fmap GD.divideComponent $ sequence $ take 22 $ repeat getLine
  O.sendOrderString $ detNextOrder gd epos' gi
  if (TN.isFinalTurn $ GD.getTurnNumber gd)
    then do
      hFlush stdout
      return ()
    else do
      hFlush stdout
      mainLoop (gd:gds) gi epos'
  where
    epos' = GP.guessEnemyPositions gi gds epos

--detNextOrder :: [GD.GameData] -> P.EnemyPosition -> GI.GameInformation -> O.Order
--detNextOrder ((GD.GameData tn ss bs):gds) epos' gi = case tn of                 -- 最初の3ピリオド（18ターンは決め打）
detNextOrder :: GD.GameData -> P.EnemyPosition -> GI.GameInformation -> O.Order
detNextOrder (GD.GameData tn ss bs) epos' gi = case tn of                 -- 最初の3ピリオド（18ターンは決め打）
  0  -> O.Order W.Spear [A.Occupy D.East, A.Move D.East]
  1  -> O.reverseOrder gi $ O.Order W.Spear [A.Occupy D.East, A.Move D.East]
  2  -> O.Order W.Axe [A.Move D.South, A.Move D.West, A.Move D.West]
  3  -> O.reverseOrder gi $ O.Order W.Axe [A.Move D.South, A.Move D.West, A.Move D.West]
  4  -> O.Order W.Swords [A.Move D.East, A.Move D.East, A.Move D.East]
  5  -> O.reverseOrder gi $ O.Order W.Swords [A.Move D.East, A.Move D.East, A.Move D.East]
  6  -> O.Order W.Spear [A.Move D.East, A.Move D.East, A.Move D.East]
  7  -> O.reverseOrder gi $ O.Order W.Spear [A.Move D.East, A.Move D.East, A.Move D.East]
  8  -> O.Order W.Axe [A.Move D.South, A.Occupy D.West]
  9  -> O.reverseOrder gi $ O.Order W.Axe [A.Move D.South, A.Occupy D.West]
  10 -> O.Order W.Swords [A.Move D.East, A.Move D.East, A.Move D.East]
  11 -> O.reverseOrder gi $ O.Order W.Swords [A.Move D.East, A.Move D.East, A.Move D.East]
  12 -> O.Order W.Spear [A.Move D.South, A.Occupy D.West]
  13 -> O.reverseOrder gi $ O.Order W.Spear [A.Move D.South, A.Occupy D.West]
  14 -> O.Order W.Axe [A.Move D.South, A.Occupy D.West]
  15 -> O.reverseOrder gi $ O.Order W.Axe [A.Move D.South, A.Occupy D.West]
  16 -> O.Order W.Swords [A.Move D.South, A.Occupy D.South]
  17 -> O.reverseOrder gi $ O.Order W.Swords [A.Move D.South, A.Occupy D.South]
  _  -> if (not . null) willAttackOrders -- 敵を攻撃する命令があるか？
       then HS.addHideOrShow ss (head willAttackOrders)
       else 
         if (not . null) notAttackedOrder    -- 占領命令の中で敵に攻撃されない命令があるか？
         then HS.addHideOrShow ss (head notAttackedOrder)
         else
           HS.addHideOrShow ss (head attackOrders')
  where
    actors = CAS.canActionFriend ss                    -- 行動可能な侍の（軍，武器）のリスト
    attackOrders = GO.oms $ map (\(_,y) -> y) actors   -- 占領命令を含む命令のリスト
    -- その内正しいmoveを行う命令のリスト --
    validAttackOrders = filter (\(O.Order w as) -> VM.validMove (SS.getSamuraiPosition (Army.Friend,w) ss) as w gi) attackOrders
    -- さらにその中で敵を攻撃する命令のリスト --
    willAttackOrders = filter (\(O.Order w as) -> AA.isAttackOrder3 epos' (SS.getSamuraiPosition (Army.Friend,w) ss) (O.Order w as)) validAttackOrders
    spearOrder = O.reverseOrder gi $ O.Order W.Spear [A.Move D.South, A.Occupy D.West] -- 槍だけは基本的にこの命令を実行
    attackOrders' = if (Army.Friend,W.Spear) `elem` actors then spearOrder:validAttackOrders else validAttackOrders
    notAttackedOrder = filter (\order -> DP.willBeAttacked ss order epos' gi) attackOrders'
{-
TODO::
 1. list-up available one order from current GameData with the following evaluation strategy.
    Evaluation Strategy
     e.g.
     * check existence of attackableEnemy. (if yes, return [Order])
     * size of occupation area
     * whether some Friend is in danger or not.
-}

-- inputExample :: [String]
-- inputExample =
--   ["14",
--    "0 6 1 1 0",
--    "1 14 0 1 0",
--    "9 12 0 1 0",
--    "-1 -1 1 0 0",
--    "-1 -1 0 0 0",
--    "-1 -1 1 0 0",
--    "0 9 9 9 9 9 9 2 9 9 9 9 9 9 9",
--    "8 9 9 9 9 9 9 9 9 9 9 9 9 9 9",
--    "8 8 9 9 9 9 9 9 9 9 9 9 9 9 9",
--    "8 8 8 9 9 9 9 9 9 9 9 9 9 9 9",
--    "8 8 8 8 9 9 9 9 9 9 9 9 9 9 9",
--    "8 0 0 0 0 9 9 9 9 9 9 9 9 9 9",
--    "0 8 8 8 8 8 9 9 9 9 9 9 9 9 9",
--    "1 8 8 8 8 9 9 9 9 8 9 9 9 9 4",
--    "0 8 8 8 9 9 9 9 8 8 3 9 9 9 9",
--    "0 8 8 9 9 9 9 8 8 8 8 8 9 9 9",
--    "8 8 8 9 9 9 8 8 8 8 8 8 8 9 9",
--    "8 8 8 8 9 8 8 8 2 8 2 8 8 8 9",
--    "1 1 8 8 8 8 8 8 2 2 2 8 8 8 8",
--    "1 1 1 8 8 8 8 8 2 2 2 8 8 8 9",
--    "8 1 1 1 8 8 8 5 2 2 2 8 8 9 3"]

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
