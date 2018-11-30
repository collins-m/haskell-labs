repFirst :: Eq a => a -> a -> [a] -> [a]
repFirst a b [] = []
repFirst a b (h:t)
    | h == a = b:t
    | otherwise = h : repFirst a b t
