module CanActionSamurai where

import qualified GameData as G
import qualified Weapon as W
import qualified Army as A
import qualified OrderStatus as OS
import qualified SamuraiStates as SS

-- data GameData = GameData { getTurnNumber :: T.TurnNumber,
--                            getSamuraiStates :: S.SamuraiStates,
--                            getBattlefieldState :: B.BattlefieldState }

-- 次のターンに行動可能な敵を列挙
canActionEnemy :: SS.SamuraiStates -> [(A.Army,W.Weapon)]
canActionEnemy ss = filter (\x -> OS.Yet == SS.getSamuraiOrderStatus x ss) [(A.Enemy,W.Spear), (A.Enemy,W.Swords), (A.Enemy,W.Axe)]
--canActionEnemy :: SS.SamuraiStates -> [W.Weapon]
--canActionEnemy ss = map snd $ filter (\x -> OS.Yet == SS.getSamuraiOrderStatus x ss) [(A.Enemy,W.Spear), (A.Enemy,W.Swords), (A.Enemy,W.Axe)]

-- 現在のターンに行動可能な味方を列挙
canActionFriend :: SS.SamuraiStates -> [(A.Army,W.Weapon)]
canActionFriend ss = filter (\x -> OS.Yet == SS.getSamuraiOrderStatus x ss) [(A.Friend,W.Spear), (A.Friend,W.Swords), (A.Friend,W.Axe)]
