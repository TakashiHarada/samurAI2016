module HomePosition where

import Position
import Weapon
import Army
import GameInformation

getInitHomePosition :: GameInformation -> (Position,Position,Position)
getInitHomePosition gi = ((\(a:b:[c]) -> (a,b,c)) $ map (\w -> getHomePosition Enemy w gi) [Spear, Swords, Axe])
  
getHomePosition :: Army -> Weapon -> GameInformation -> Position
getHomePosition Friend Spear  First  = friendSpearHome
getHomePosition Friend Spear  Second = enemySpearHome
getHomePosition Friend Swords First  = friendSwordsHome
getHomePosition Friend Swords Second = enemySwordsHome
getHomePosition Friend Axe    First  = friendAxeHome
getHomePosition Friend Axe    Second = friendAxeHome
getHomePosition Enemy Spear   First  = enemySpearHome
getHomePosition Enemy Spear   Second = friendSpearHome
getHomePosition Enemy Swords  First  = enemySwordsHome
getHomePosition Enemy Swords  Second = friendSwordsHome
getHomePosition Enemy Axe     First  = enemyAxeHome
getHomePosition Enemy Axe     Second = friendAxeHome

-- 先攻の場合の居館
friendSpearHome :: Position 
friendSpearHome = (0,0)

friendSwordsHome :: Position 
friendSwordsHome = (0,7)

friendAxeHome :: Position 
friendAxeHome   = (7,0)

enemySpearHome :: Position
enemySpearHome = (14,14)

enemySwordsHome :: Position
enemySwordsHome = (14,7)

enemyAxeHome :: Position
enemyAxeHome = (7,14)
