module OrderStatus where

data OrderStatus = Already | Yet deriving (Show,Eq,Ord)

orderStatusToInt :: OrderStatus -> Int
orderStatusToInt Already = 1
orderStatusToInt Yet = 0

intToOrderStatus :: Int -> OrderStatus
intToOrderStatus 1 = Already
intToOrderStatus 0 = Yet
intToOrderStatus _ = Already
