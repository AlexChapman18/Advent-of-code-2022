import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type Assignment = (Int, Int)
type Pair = (Assignment, Assignment)

-- Reads in the file
intakeFile :: FilePath -> IO Int
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod) pairs
    pairs = fmap (map lineToPair) rawLines
    rawLines = fmap (lines) (readFile filePath)

-- Converts the strings to ints
lineToPair :: String -> Pair
lineToPair line = pair
  where
    pair = ((getInt 0 , getInt 1) , (getInt 2 , getInt 3))
    getInt n = read (splitLine !! n) :: Int
    splitLine = words (map (nonDigit) line)
    nonDigit = (\x -> if isDigit x then x else ' ')


-- Main method
mainMethod :: [Pair] -> Int
mainMethod pairs = total
  where
    total = sum (map (isFullyContained) pairs)

-- If 1 Elfs assignment are container withing another return 1, else 0
isFullyContained :: Pair -> Int
isFullyContained ((a,b), (x,y)) | or [isAsubX, isXsubA] = 1
                                | otherwise = 0
  where
    isAsubX = and [a >= x, b <= y]
    isXsubA = and [x >= a, y <= b]