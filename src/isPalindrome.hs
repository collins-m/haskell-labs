rev :: Eq a => [a] -> [a]
rev [] = []
rev (h:[]) = [h]
rev (h:t) = rev t ++ [h]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (h:t) = (h:t) == rev (h:t)