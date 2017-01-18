-- $ hlint hogePlayer.hs

import Control.Monad
import System.IO
import qualified TurnInformation as T
import qualified TurnNumber as TN
import qualified Ordering as O
import qualified GameData as G
       
main :: IO ()
main = do
  -- get a Game Information and response 0
  gameInfo <- getLine
  putStrLn "0"
  hFlush stdout
  -- get a Turn Information and response
  mainLoop []
  return ()

mainLoop :: [G.GameData] -> IO()
mainLoop gds = do
  tnum <- getLine
  s <- getContents
  let ls = lines s
      gd = G.getGameData ls
      gameRecords = gd : gds
  O.sendOrderString $ detNextOrder gameRecords
--  if (TN.isFinalTurn $ TN.tn gd)
  if (TN.isFinalTurn $ TN.getTurnNumber tnum)
    then do
      hFlush stdout
      return ()
    else do
      hFlush stdout
      mainLoop (gameRecords)

-- TODO:: Implement!
detNextOrder :: [G.GameData] -> O.Order
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
