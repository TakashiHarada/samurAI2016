module GetShowingEnemy where

import qualified ShowingStatus as S
import qualified SamuraiStates as SS
import qualified Weapon as W
import qualified Army as A

-- 現在のターンで顕現している敵の侍を列挙
getShowingEnemy :: SS.SamuraiStates -> [(A.Army,W.Weapon)]
getShowingEnemy ss = filter (\x -> S.Show == SS.getSamuraiShowingStatus x ss) [(A.Enemy,W.Spear), (A.Enemy,W.Swords), (A.Enemy,W.Axe)]
-- getShowingEnemy :: SS.SamuraiStates -> [W.Weapon]
-- getShowingEnemy ss = map snd $ filter (\x -> S.Show == SS.getSamuraiShowingStatus x ss) [(A.Enemy,W.Spear), (A.Enemy,W.Swords), (A.Enemy,W.Axe)]
