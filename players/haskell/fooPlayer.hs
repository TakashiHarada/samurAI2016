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

main :: IO ()
main = do
  gameInfo <- GI.readGameInformation
  GI.respondToTheGameInformation 
  mainLoop [] gameInfo (HP.getInitHomePosition gameInfo)
  return ()

mainLoop :: [GD.GameData] -> GI.GameInformation -> P.EnemyPosition -> IO()
mainLoop gds gi epos = do
  gd <- fmap GD.divideComponent $ sequence $ take 22 $ repeat getLine
  O.sendOrderString $ detNextOrder (gd:gds) epos gi
  if (TN.isFinalTurn $ GD.getTurnNumber gd)
    then do
      hFlush stdout
      return ()
    else do
      hFlush stdout
      mainLoop (gd:gds) gi epos

detNextOrder :: [GD.GameData] -> P.EnemyPosition -> GI.GameInformation -> O.Order
detNextOrder ((GD.GameData tn ss bs):gds) (e1,e2,e3) gi = case tn of                 -- 最初の2ピリオド（6ターンは決め打）
  0 -> O.reverseOrder gi $ O.Order W.Spear  [A.Move D.East, A.Occupy D.South]
  1 -> O.reverseOrder gi $ O.Order W.Swords [A.Move D.East, A.Occupy D.South]
  2 -> O.reverseOrder gi $ O.Order W.Axe [A.Occupy D.West, A.Move D.East]
  3 -> O.reverseOrder gi $ O.Order W.Spear  [A.Move D.East, A.Occupy D.South]
  4 -> O.reverseOrder gi $ O.Order W.Swords [A.Move D.East, A.Occupy D.South]
  5 -> O.reverseOrder gi $ O.Order W.Axe [A.Occupy D.West, A.Move D.East]
  _ -> if (not . null) willAttackOrder -- 敵を攻撃する命令があるか？
       then head validAttackOrders
       else
         if undefined    -- 敵に攻撃される命令があるか？
         then undefined
         else undefined
    where
      epos'
        = (GP.guessPosition W.Spear ((GD.GameData tn ss bs):gds) e1,   -- 敵の槍の位置を推測
           GP.guessPosition W.Swords ((GD.GameData tn ss bs):gds) e2,  -- 敵の剣の位置を推測
           GP.guessPosition W.Axe ((GD.GameData tn ss bs):gds) e3)     -- 敵の斧の位置を推測
      actors = CAS.canActionFriend ss                    -- 行動可能な侍の（軍，武器）のリスト
      attackOrders = GO.oms $ map (\(_,y) -> y) actors   -- 占領命令を含む命令のリスト
      -- その内正しいmoveを行う命令のリスト --
      validAttackOrders = filter (\(O.Order w as) -> VM.validMove (SS.getSamuraiPosition (Army.Friend,w) ss) as w gi) attackOrders
      -- さらにその中で敵を攻撃する命令のリスト --
      willAttackOrder = filter (\(O.Order w as) -> AA.isAttackOrder3 epos' (SS.getSamuraiPosition (Army.Friend,w) ss) (O.Order w as)) validAttackOrders 
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
