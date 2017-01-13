module Ordering where

data Friend = Spear | Swords | Axe
data Action =
  OcuS | OcuE | OcuN | OcuW |
  MovS | MovE | MovN | MovW |
  Show | Hyde deriving (Show,Eq,Ord)

data Order = Order { actor :: Friend, actions :: Action}
