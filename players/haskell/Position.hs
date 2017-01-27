module Position where

type Position = (Int,Int)

removeOutOfBoard :: [Position] -> [Position]
removeOutOfBoard = Prelude.filter (\(s,t) -> s <= 14 && t <= 14 && 0 <= s && 0 <= t)

