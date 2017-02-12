-- $ hlint hogePlayer.hs

import System.IO
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
import qualified SearchAttackAdvantage as SAA

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
         if (not . null) adv6
         then
           if (not . null) notAttackedOrder6
           then HS.addHideOrShow ss (head notAttackedOrder6)
           else HS.addHideOrShow ss (head adv6)
         else
           if (not . null) adv4
           then
             if (not . null) notAttackedOrder4
             then HS.addHideOrShow ss (head notAttackedOrder4)
             else HS.addHideOrShow ss (head adv4)
           else
             HS.addHideOrShow ss (head attackOrders')
  where
    actors = CAS.canActionFriend ss -- 行動可能な侍の（軍，武器）のリスト
    -- 侍の位置のリスト（位置がわかるものだけ：隱伏している敵の侍の位置は含まない）
    explicitPos = map (\(a,w) -> SS.getSamuraiPosition (a,w) ss) (friendsW ++ CAS.canActionEnemy ss)
    friendsW = [(Army.Friend,w) | w <- [W.Spear,W.Swords,W.Axe]]
    attackOrders = GO.oms $ map (\(_,y) -> y) actors  -- 占領命令を含む命令のリスト
    -- その内正しいmoveを行う命令のリスト --
    validAttackOrders = filter (\(O.Order w as) -> VM.validMove (SS.getSamuraiPosition (Army.Friend,w) ss) as w gi explicitPos) attackOrders
    -- さらにその中で敵を攻撃する命令のリスト --
    willAttackOrders = filter (\(O.Order w as) -> AA.isAttackOrder3 epos' (SS.getSamuraiPosition (Army.Friend,w) ss) (O.Order w as)) validAttackOrders
    spearOrder = O.reverseOrder gi $ O.Order W.Spear [A.Move D.South, A.Occupy D.West] -- 槍だけは基本的にこの命令を実行
    attackOrders' = if (Army.Friend,W.Spear) `elem` actors then spearOrder:validAttackOrders else validAttackOrders
    adv6 = filter (\(O.Order w as) -> SAA.searchAttackAdvantage bs (SS.getSamuraiPosition (Army.Friend,w) ss) (O.Order w as) > 5) attackOrders' 
    adv4 = filter (\(O.Order w as) -> SAA.searchAttackAdvantage bs (SS.getSamuraiPosition (Army.Friend,w) ss) (O.Order w as) > 3) attackOrders' 
    notAttackedOrder6 = filter (\order -> DP.willBeAttacked ss order epos' gi) adv6
    notAttackedOrder4 = filter (\order -> DP.willBeAttacked ss order epos' gi) adv4
    
-- foo = GD.divideComponent inputExample
-- foo2 = (GD.getSamuraiStates foo)
-- foo3 = filter (\(O.Order w as) -> VM.validMove (SS.getSamuraiPosition (Army.Friend,w) foo2) as w GI.First [(8,6),(7,4)]) $  GO.oms $ map (\(_,y) -> y) $ CAS.canActionFriend foo2
-- foo4 = filter (\(O.Order w as) -> AA.isAttackOrder3 ((7,10),(-1,-1),(3,11)) (SS.getSamuraiPosition (Army.Friend,w) foo2) (O.Order w as)) foo3
-- GO.oms $ map (\(_,y) -> y) $ CAS.canActionFriend foo2

{-
TODO::
 1. list-up available one order from current GameData with the following evaluation strategy.
    Evaluation Strategy
     e.g.
     * check existence of attackableEnemy. (if yes, return [Order])
     * size of occupation area
     * whether some Friend is in danger or not.
-}
-- SS.getSamuraiPosition (Army.Enemy,W.Axe) (GD.getSamuraiStates foo)
-- AA.attackArea (8,6) (O.Order W.Swords [A.Move D.North, A.Occupy D.North])
-- AA.attackArea (8,6) (O.Order W.Swords [A.Occupy D.South, A.Move D.North])


-- inputExample :: [String]
-- inputExample =
--   ["25",
--    "4 9 0 0 0",
--    "6 14 0 0 0",
--    "5 11 0 0 0",
--    "7 10 0 0 0",
--    "-1 -1 0 1 0",
--    "3 11 1 0 0",
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
