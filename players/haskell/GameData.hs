module GameData where

import System.IO
import qualified TurnNumber as T
import qualified SamuraiStates as S
import qualified ShowingStatus as SS
import qualified BattlefieldState as B
import qualified BattlefieldSection as BS
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
divideComponent s = GameData
                    (stringToTurnNumber turnNumberString)
                    (M.fromList (stringsToSamuraiStates samuraiStateStrings))
                    (stringsToBattlefieldState battlefieldString)
  where
    ls = lines s
    turnNumberString = head ls
    samuraiStateStrings = (take 6 . tail) ls
    battlefieldString = (words . unlines . drop 7) ls                

hh = "-1 -1 1 0 0\n-1 -1 0 0 0\n-1 -1 1 0 0\n0 0 0 0\n1 1 1 1\n2 2 2 2"

stringToTurnNumber :: String -> T.TurnNumber
stringToTurnNumber str =  digitToInt $ read str :: Int 

stringsToSamuraiStates :: [String] -> [((A.Army,W.Weapon),S.SamuraiState)]
stringsToSamuraiStates str =  zip
             [ (x,y) | x <- [A.Friend,A.Enemy], y <- [W.Spear,W.Swords,W.Axe]]
             [ S.SamuraiState
               (list2pair $ map read $ take 2 $ words $ str !! pos)
               (O.intToOrderStatus $ read $ (words $ str !! pos) !! 2)
               (SS.intToShowingStatus $ read $ (words $ str !! pos) !! 3)
               (read $ (words $ str !! pos) !! 4)
             | pos <- [0..5]]
  where
    list2pair = \[x,y] -> (x,y)

stringsToBattlefieldState :: [String] -> B.BattlefieldState
stringsToBattlefieldState ss = M.fromList $ zip [(x,y) | x <- [0..14], y <- [0..14]] (map BS.stringToBattlefieldSection  ss)
