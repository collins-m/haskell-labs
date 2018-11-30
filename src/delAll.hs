delAll :: Eq a => a -> [a] -> [a]
delAll a [] = []
delAll a (h:t)
    | h == a = delAll a t
    | otherwise = h : delAll a t
