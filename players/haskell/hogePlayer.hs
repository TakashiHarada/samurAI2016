-- $ hlint hogePlayer.hs

import Control.Monad
import System.IO
import qualified TurnInformation as T
import qualified Ordering as O
       
main :: IO ()
main = do
  -- get a Game Information and response 0
  gameInfo <- getLine
  putStrLn "0"
  hFlush stdout
  -- get a Turn Information and response
  mainLoop []
  return ()

mainLoop :: [T.GameData] -> IO()
mainLoop gds = do
  s <- getContents
  let ls = lines s
      gd = getGameData ls
      gameRecords = gd : gds
  O.sendOrderString $ detNextOrder gameRecords
  if (T.isFinalTurn $ T.tnum gd)
    then
      return ()
    else do
      hFlush stdout
      mainLoop (gameRecords)

getGameData :: [String] -> T.GameData
getGameData ls = T.iniData --FIXME
-- tn = getTurnNumber (head ls)

getTurnNumber :: String -> T.TurnNumber
getTurnNumber s = read s

-- TODO:: Implement!
detNextOrder :: [T.GameData] -> [O.Order]
detNextOrder gds = undefined



{-
getTurnInfo :: String -> IO()
getTurnInfo x = do
  turnNumber <- getLine
  friend1 <- getLine
  friend2 <- getLine
  friend3 <- getLine
  return ()
  enemy1 <- getLine
  enemy2 <- getLine
  enemy3 <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  _ <- getLine
  if (turnNumber == 96) then putStrLn "1 1 6 9 0"
                        else putStrLn "1 1 6 9 0"
                             >>= \_ -> hFlush stdout >>= \_ -> getTurnInfo x
-}
