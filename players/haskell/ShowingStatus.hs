module ShowingStatus where

data ShowingStatus = Show | Hide | Unkown deriving (Show,Eq,Ord)

-- For enemies, you cannot tell whether they are hiding or simply out of the vision.  The showing status of 1 is given in either case.

showingStatusToInt :: ShowingStatus -> Int
showingStatusToInt Show   = 1  -- friendly samurai hiding
showingStatusToInt Hide   = 0  -- friendly samurai showing
showingStatusToInt Unkown = 1  -- for enemy samurai
