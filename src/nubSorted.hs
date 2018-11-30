nubSorted :: Eq a => [a] -> [a]
nubSorted [] = []
nubSorted f:[] = f:[]
nubSorted (f:s:t)
    | f == s = f : nubSorted t
    | otherwise = f : nubSorted s:t