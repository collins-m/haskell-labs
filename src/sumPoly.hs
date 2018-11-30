sumPoly :: [Int] -> [Int] -> [Int]
sumPoly [] [] = []                  -- No vals
sumPoly (h1:[]) (h2:[])  = [h1+h2]  -- reg case
sumPoly (h1:[]) [] = [h1]           -- first is longer
sumPoly [] (h2:[]) = [h2]           -- second is longer
sumPoly (h1:t1) (h2:t2) = [h1+h2] ++ sumPoly (t1) (t2)