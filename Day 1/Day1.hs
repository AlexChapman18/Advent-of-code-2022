import System.IO
import Control.Monad


intakeFile :: FilePath -> Int -> IO [Int]
intakeFile filePath n = fmap (getMostCalories 3) formattedValues
  where
    formattedValues = fmap (lines) (readFile filePath)

getMostCalories :: Int -> [String] -> [Int]
getMostCalories n inputList = getTopNValues formattedValues n
  where
    formattedValues = sumSections 0 formattedInput
    formattedInput = map (stringToInt) inputList

getTopNValues :: [Int] -> Int -> [Int]
getTopNValues values 0 = []
getTopNValues values n = maximumA values : getTopNValues (removeValue values (maximumA values)) (n-1)

sumSections :: Int -> [Int] -> [Int]
sumSections currentTotal [] = [currentTotal]
sumSections currentTotal (-1:rest) = currentTotal : (sumSections 0 rest)
sumSections currentTotal (top:rest) = sumSections (currentTotal + top) rest

maximumA :: [Int] -> Int
maximumA [] = 0
maximumA (x:xs) | (maximumA xs) > x = (maximumA xs)
                | otherwise = x

removeValue :: [Int] -> Int -> [Int]
removeValue [] value = []
removeValue (x:xs) value | x == value = 0 : xs
                         | otherwise = x : removeValue xs value

stringToInt :: String -> Int
stringToInt "" = -1
stringToInt value = read value :: Int