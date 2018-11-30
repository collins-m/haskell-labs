evalPoly :: Int -> [Int] -> Int
evalPoly i [] = 0
evalPoly i (h:t) = h + i * (evalPoly i t)