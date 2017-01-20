module BattlefieldSection where

import qualified Army as A
import qualified Weapon as W

data BattlefieldSection = Occupied A.Army W.Weapon | NotOccupied | NoInformation deriving (Show,Eq,Ord)

battlefieldSectionToString :: BattlefieldSection -> String
battlefieldSectionToString (Occupied A.Friend W.Spear) = "0"
battlefieldSectionToString (Occupied A.Friend W.Swords) = "1"
battlefieldSectionToString (Occupied A.Friend W.Axe) = "2"
battlefieldSectionToString (Occupied A.Enemy W.Spear) = "3"
battlefieldSectionToString (Occupied A.Enemy W.Swords) = "4"
battlefieldSectionToString (Occupied A.Enemy W.Axe) = "5"
battlefieldSectionToString NotOccupied = "8"
battlefieldSectionToString NoInformation = "9"

stringToBattlefieldSection :: String -> BattlefieldSection
stringToBattlefieldSection "0" = (Occupied A.Friend W.Spear)
stringToBattlefieldSection "1" = (Occupied A.Friend W.Swords)
stringToBattlefieldSection "2" = (Occupied A.Friend W.Axe)
stringToBattlefieldSection "3" = (Occupied A.Enemy W.Spear)
stringToBattlefieldSection "4" = (Occupied A.Enemy W.Swords)
stringToBattlefieldSection "5" = (Occupied A.Enemy W.Axe)
stringToBattlefieldSection "6" = NotOccupied
stringToBattlefieldSection "9" = NoInformation
stringToBattlefieldSection _   = error "Error"
