shortest :: [[a]] -> [a]
shortest [] = []
shortest [a] = a
shortest (f:s:t)
    | length f < length s = shortest (f:t)
    | otherwise = shortest (s:t)   