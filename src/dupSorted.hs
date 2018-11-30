dupSorted :: Eq => [a] -> Bool
dupSorted [] = False
dupSorted f:[] = False
dupSorted (f:s:t)
    | f == s = True
    | otherwise = dupSorted s:t