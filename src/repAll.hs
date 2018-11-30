repAll :: Eq a => a -> a -> [a] -> [a]
repAll a b [] = []
repAll a b (h:t)
    | h == a = b : repAll a b t
    | otherwise = h : repAll a b t
