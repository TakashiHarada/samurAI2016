module GameData where

import qualified TurnNumber as T
import qualified SamuraiStates as S
import qualified BattlefieldState as B
import qualified Weapon as W
import qualified Army as A
import qualified Data.Map as M


data GameData = GameData { getTurnNumber :: T.TurnNumber,
                           getSamuraiStates :: S.SamuraiStates,
                           getBattlefieldState :: B.BattlefieldState } deriving (Show,Eq,Ord)

divideComponent :: String -> GameData
divideComponent = undefined

--divideComponent = \s -> GameData (getTN s) (M.fromList (getSS s)) (M.fromList [getBS s])

getTN :: String -> T.TurnNumber
getTN s = read $ head $ lines s

getSS :: String -> [((A.Army,W.Weapon),S.SamuraiState)]
getSS = undefined
--getSS s = zip [ (x,y) | x <- A.Army, y <- W.Weapon]  S.SamuraiState ((,)) () () () 
--      where
--       info = words tail lines s

getBS :: String -> B.BattlefieldState
getBS = undefined
