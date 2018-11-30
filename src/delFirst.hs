delFirst :: Eq a => a -> [a] -> [a]
delFirst a [] = []
delFirst a (h:t)
    | h == a = t
    | otherwise = h : delFirst a t
