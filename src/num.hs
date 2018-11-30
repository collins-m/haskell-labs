num :: Eq a => a -> [a] -> Int
num a [] = 0
num a (h:t)
    | h == a = 1 + num a t
    | otherwise = 0 + num a t
