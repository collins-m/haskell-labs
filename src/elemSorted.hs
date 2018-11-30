elemSorted :: Ord a => a -> [a] -> Bool
elemSorted a [] = False
elemSorted a (h:t)
    | h == a = True
    | h > a = False
    | otherwise = elemSorted a t