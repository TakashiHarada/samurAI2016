module GameInformation where

import System.IO

data GameInformation = First | Second deriving (Show,Eq,Ord)

readGameInformation :: IO GameInformation
readGameInformation = do
  b <- getLine
  if (b == "0")
    then return First
    else return Second

respondToTheGameInformation :: IO ()
respondToTheGameInformation = putStrLn "0" >>= \_ -> hFlush stdout
