module CurrentPosition where

type CurrentPosition = (Int,Int)

removeOutOfBoard :: [CurrentPosition] -> [CurrentPosition]
removeOutOfBoard = Prelude.filter (\(s,t) -> s <= 14 && t <= 14 && 0 <= s && 0 <= t)

