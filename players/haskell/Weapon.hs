module Weapon where

data Weapon = Spear | Swords | Axe deriving (Show,Eq,Ord,Enum)

weaponID :: Weapon -> Int
weaponID Spear = 0
weaponID Swords = 1
weaponID Axe = 2
