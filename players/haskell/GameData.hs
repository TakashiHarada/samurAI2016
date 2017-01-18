module GameData where

import System.IO
import qualified TurnNumber as T
import qualified SamuraiStates as S
import qualified ShowingStatus as SS
import qualified BattlefieldState as B
import qualified OrderStatus as O
import qualified Weapon as W
import qualified Army as A
import qualified Data.Map as M
import Data.Char

data GameData = GameData { getTurnNumber :: T.TurnNumber,
                           getSamuraiStates :: S.SamuraiStates,
                           getBattlefieldState :: B.BattlefieldState } deriving (Show,Eq,Ord)

readGameInfo = getLine
acknowledgementResponseToTheGameInformation :: IO ()
acknowledgementResponseToTheGameInformation = putStrLn "0" >>= \_ -> hFlush stdout

divideComponent :: String -> GameData
--divideComponent = undefined
--divideComponent s = GameData (getTN turnNumberString) (M.fromList (getSS samuraiStateString)) (M.fromList [getBS battlefieldString])
divideComponent s = GameData (getTN turnNumberString) (M.fromList (getSS undefined)) (getBS undefined)
      where
       ls = lines s
       turnNumberString = head ls
       samuraiStateString = (take 6 . tail) ls
       battlefieldString = drop 7 ls                


getTN :: String -> T.TurnNumber
getTN str =  digitToInt $ read str :: Int 


getSS :: [String] -> [((A.Army,W.Weapon),S.SamuraiState)]
-- getSS = undefined
getSS str =  zip
             [ (x,y) | x <- [A.Friend,A.Enemy], y <- [W.Spear,W.Swords,W.Axe]]
             [ S.SamuraiState
               (list2pair $ map read $ take 2 $ words $ str !! pos)
               (O.intToOrderStatus $ read $ (words $ str !! pos) !! 2)
               (SS.intToShowingStatus $ read $ (words $ str !! pos) !! 3)
               (read $ (words $ str !! pos) !! 4)
             | pos <- [0..5]]
--       (flip (!!) 2 $ func pos str)
--       (flip (!!) 3 $ func pos str)
--       (flip (!!) 4 $ func pos str)  | pos <- [0..5]]
    where
--       func = words $ flip (!!)
       list2pair = \[x,y] -> (x,y)

getBS :: [String] -> B.BattlefieldState
getBS = undefined
--getBS s  = map (\[x,y] -> (x,y).(map read) .  take 2 .  words . lines) s

