import Control.Monad
import System.IO
import TurnInformation
  
main :: IO ()
main = do
  -- get a Game Information and response 0
  gameInfo <- getLine
  putStrLn "0";
  hFlush stdout
  -- get a Turn Information and response
  getTurnInfo gameInfo

getTurnInfo :: String -> IO ()
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
  if (turnNumber == "96") then putStrLn "1 1 6 9 0" else putStrLn "1 1 6 9 0" >>= \_ -> hFlush stdout >>= \_ -> getTurnInfo x
