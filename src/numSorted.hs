numSorted :: Ord a => a -> [a] -> Int
numSorted a [] = 0
numSorted a (h:t)
    | h == a = 1 + numSorted a t
    | h > a = 0
    | otherwise = numSorted a t
