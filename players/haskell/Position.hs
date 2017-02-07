module Position where

type Position = (Int,Int)
type EnemyPosition = (Position,Position,Position)

removeOutOfBoard :: [Position] -> [Position]
removeOutOfBoard = Prelude.filter (\(s,t) -> s <= 14 && t <= 14 && 0 <= s && 0 <= t)
