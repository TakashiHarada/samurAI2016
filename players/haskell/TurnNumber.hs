module TurnNumber where

type TurnNumber = Int

final1 :: TurnNumber
final1 = 94

final2 :: TurnNumber
final2 = 95

isFinalTurn :: TurnNumber -> Bool
isFinalTurn x = x == final1 || x == final2

-- readTurnNumber :: String -> TurnNumber
-- readTurnNumber s = read s
