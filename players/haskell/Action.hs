module Action where

import qualified Direction as D

data Action = Occupy D.Direction | Move D.Direction | Show | Hyde
  deriving (Show,Eq,Ord)
