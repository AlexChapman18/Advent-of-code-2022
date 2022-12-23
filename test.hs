import Data.Char

hasMark :: String -> [String] -> Bool
hasMark _ [] = False
hasMark name (n:rest)
   | name == n = True
   | otherwise = hasMark name rest