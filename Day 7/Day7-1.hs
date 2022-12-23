import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
data Size = Int
data File = (String, Size)
data Directory = Node [Directory] [File] Size deriving (Show)

-- Reads in the file
intakeFile :: FilePath -> IO Int
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod) rawLines
    rawLines = fmap (lines) (readFile filePath)


buildTree :: [String] -> Tree
buildTree

-- Main method
mainMethod :: [String] -> Int
mainMethod rawLine = -1
