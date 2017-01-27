-- $ hlint hogePlayer.hs

import Control.Monad
import System.IO
import qualified TurnInformation as T
import qualified TurnNumber as TN
import qualified Ordering as O
import qualified GameInformation as GI
import qualified GameData as GD
import qualified Weapon as W
import qualified Action as A
import qualified Direction as D
       
main :: IO ()
main = do
  gameInfo <- GI.readGameInformation
  GI.respondToTheGameInformation 
  mainLoop []
  return ()

mainLoop :: [GD.GameData] -> IO()
mainLoop gds = do
  gd <- fmap GD.divideComponent $ sequence $ take 22 $ repeat getLine
  O.sendOrderString $ detNextOrder (gd:gds)
  if (TN.isFinalTurn $ GD.getTurnNumber gd)
    then do
      hFlush stdout
      return ()
    else do
      hFlush stdout
      mainLoop (gd:gds)

-- TODO:: Implement!
detNextOrder :: [GD.GameData] -> O.Order
detNextOrder gds = O.Order W.Spear [A.Occupy D.South]
--detNextOrder gds = O.Order W.Spear [A.Occupy D.South, A.Move D.North]

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
