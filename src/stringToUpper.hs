import Data.Char(toUpper)

stringToUpper :: String -> String
stringToUpper "" = ""
stringToUpper (h:t) = toUpper h : stringToUpper t
