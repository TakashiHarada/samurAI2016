module Position where

type Position = (Int,Int)

removeOutOfBoard :: [Position] -> [Position]
removeOutOfBoard = Prelude.filter (\(s,t) -> s <= 14 && t <= 14 && 0 <= s && 0 <= t)

fSpear :: Position 
fSpear = (0,0)

fSwords :: Position 
fSwords = (0,7)

fAxe :: Position 
fAxe   = (7,0)

eSpear :: Position
eSpear = (14,14)

eSword :: Position
eSword = (14,7)

eAxe :: Position
eAxe = (7,14)
