module BattlefieldSection where

import qualified Army as A
import qualified Weapon as W

data BattlefieldSection = Occupied A.Army W.Weapon | NotOccupied | NoInformation deriving (Show,Eq,Ord)

battlefieldSectionToInt :: BattlefieldSection -> Int
battlefieldSectionToInt (Occupied A.Friend W.Spear) = 0
battlefieldSectionToInt (Occupied A.Friend W.Swords) = 1
battlefieldSectionToInt (Occupied A.Friend W.Axe) = 2
battlefieldSectionToInt (Occupied A.Enemy W.Spear) = 3
battlefieldSectionToInt (Occupied A.Enemy W.Swords) = 4
battlefieldSectionToInt (Occupied A.Enemy W.Axe) = 5
battlefieldSectionToInt NotOccupied = 8
battlefieldSectionToInt NoInformation = 9
