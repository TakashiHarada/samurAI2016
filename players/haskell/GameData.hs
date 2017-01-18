module GameData where

import qualified TurnNumber as T
import qualified SamuraiStates as S
import qualified BattlefieldState as B

data GameData = GameData { getTurnNumber :: T.TurnNumber,
                           getSamuraiStates :: S.SamuraiStates,
                           getBattlefieldState :: B.BattlefieldState } deriving (Show,Eq,Ord)

getGameData :: [String] -> GameData
getGameData ls = undefined -- T.iniData --FIXME
