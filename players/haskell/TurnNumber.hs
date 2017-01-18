module TurnNumber where

type TurnNumber = Int

final1 = 95
final2 = 96

isFinalTurn :: TurnNumber -> Bool
isFinalTurn x = x == final1 || x == final2

-- readTurnNumber :: String -> TurnNumber
-- readTurnNumber s = read s
