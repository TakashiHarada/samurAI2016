-- $ hlint hogePlayer.hs

import Control.Monad
import System.IO
import qualified TurnInformation as T
import qualified TurnNumber as TN
import qualified Ordering as O
import qualified GameData as G
       
main :: IO ()
main = do
  gameInfo <- G.readGameInfo
  G.acknowledgementResponseToTheGameInformation 
  mainLoop []
  return ()

mainLoop :: [G.GameData] -> IO()
mainLoop gds = do
  gd <- fmap G.divideComponent getContents
  O.sendOrderString $ detNextOrder (gd:gds)
  if (TN.isFinalTurn $ G.getTurnNumber gd)
    then do
      hFlush stdout
      return ()
    else do
      hFlush stdout
      mainLoop (gd:gds)

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
