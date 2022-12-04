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

-- Converts the strings to Pairs
lineToPair :: String -> Pair
lineToPair line = pair
  where
    pair = ((getInt 0 , getInt 1) , (getInt 2 , getInt 3))
    getInt n = read (splitLine !! n) :: Int
    splitLine = traceShow ((map (nonDigit) line)) words (map (nonDigit) line)
    nonDigit = (\x -> if isDigit x then x else ' ')


-- Main method
mainMethod :: [Pair] -> Int
mainMethod pairs = total
  where
    total = sum (map (getOverlap) pairs)

-- if there is any overlap, return 1, else, 0
getOverlap :: Pair -> Int
getOverlap ((a,b), (x,y)) | length overlap > 0 = 1
                          | otherwise = 0
  where
    aAssignments = [a..b]
    xAssignments = [x..y]
    overlap = intersect aAssignments xAssignments