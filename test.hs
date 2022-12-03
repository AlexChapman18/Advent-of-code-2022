import Data.Char

charToValue :: Char -> Int
charToValue char | isUpper char = (ord char) - 38
                 | otherwise = (ord char) - 96