import System.IO
import Control.Monad
import Data.List

-- Reads in the file
intakeFile :: FilePath -> Int -> IO Int
intakeFile filePath arg1 = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod arg1) integerLines
    integerLines = fmap (map stringToInt) rawLines
    rawLines = fmap (lines) (readFile filePath)

-- Converts the strings to ints
stringToInt :: String -> Int
stringToInt "" = -1
stringToInt value = read value :: Int


-- Main method
mainMethod :: Int -> [Int] -> Int
mainMethod n rawData = sum topNValues
  where
    topNValues = take n sortedValues
    sortedValues = (reverse.sort) summedValues
    summedValues = sumGroups 0 rawData


--Sums each group of calories
sumGroups :: Int -> [Int] -> [Int]
sumGroups total [] = [total]
sumGroups total (-1:tail) = total : sumGroups 0 tail
sumGroups total (head:tail) = sumGroups (total + head) tail
