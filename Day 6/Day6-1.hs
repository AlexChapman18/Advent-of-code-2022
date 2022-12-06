import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type DataStream = String


-- Reads in the file
intakeFile :: FilePath -> IO Int
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod 4) rawLine
    rawLine = fmap (head) rawLines
    rawLines = fmap (lines) (readFile filePath)


-- Main method
mainMethod :: Int -> DataStream -> Int
mainMethod _ [] = -1
mainMethod index (start:rest) | isUnique currentStart = traceShow (currentStart) index
                            | otherwise = mainMethod (index+1) rest
  where
    currentStart = start : (take 3 rest)


isUnique :: String -> Bool
isUnique string = (length string) == (length uniqueString)
  where
    uniqueString = map (head) ((group.sort) string)
