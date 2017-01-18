module GameData where

import System.IO
import qualified TurnNumber as T
import qualified SamuraiStates as S
import qualified BattlefieldState as B
import qualified Weapon as W
import qualified Army as A
import qualified Data.Map as M


data GameData = GameData { getTurnNumber :: T.TurnNumber,
                           getSamuraiStates :: S.SamuraiStates,
                           getBattlefieldState :: B.BattlefieldState } deriving (Show,Eq,Ord)

readGameInfo = getLine
acknowledgementResponseToTheGameInformation :: IO ()
acknowledgementResponseToTheGameInformation = putStrLn "0" >>= \_ -> hFlush stdout

divideComponent :: String -> GameData
--divideComponent = undefined
divideComponent s = GameData (getTN s) (M.fromList (getSS s)) (M.fromList [getBS s])

getTN :: String -> T.TurnNumber
getTN s = (read.head.lines) s

getSS :: String -> [((A.Army,W.Weapon),S.SamuraiState)]
--getSS = undefined
getSS s = zip 
      [ (x,y) | x <- [A.Friend,A.Enemy], y <- [W.Spear,W.Swords,W.Axe]] 
      [ S.SamuraiState ((map \[x,y] -> (x,y) . (map read) .  take 2 .  words . lines s !! 0),(map \[x,y] -> (x,y) .map read .  take 2 .  words . lines s !! 1))  ((searchPosition pos) !! 2 ) ((searchPosition pos) !! 3) ((searchPosition pos) !! 4)  | pos <- [0..5] ]
      where
      searchPosition n = map (take 2 . words . tail . take 7 . lines) s

-- getSS s = zip 
--       [ (x,y) | x <- [A.Friend,A.Enemy], y <- [W.Spear,W.Swords,W.Axe]] 
--       [ z | z <- [ zz | zz <- S.SamuraiState (( (searchPosition pos) !! 0),((searchPosition pos) !! 1) ) ((searchPosition pos) !! 2 ) ((searchPosition pos) !! 3) ((searchPosition pos) !! 4) ,pos <- [0..5] ] ]
--       where
--       searchPosition n = map (take 2 . words . tail . take 7 . lines) s

getBS :: String -> B.BattlefieldState
--getBS = undefined
getBS s  = map (\[x,y] -> (x,y).(map read) .  take 2 .  words . lines) s

