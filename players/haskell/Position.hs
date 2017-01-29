module Position where

type Position = (Int,Int)

removeOutOfBoard :: [Position] -> [Position]
removeOutOfBoard = Prelude.filter (\(s,t) -> s <= 14 && t <= 14 && 0 <= s && 0 <= t)

fSpear = (0,0)
fSword = (0,7)
fAxe   = (7,0)

eSpear = (14,14)
eSword = (14,7)
eAxe   = (7,14)
