import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type Tree = (Int, Bool)
type Row = [Tree]
type Grid = [Row]


-- Reads in the file
intakeFile :: FilePath -> IO Int
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod) rawRows
    rawRows = fmap (map parseRow) rawLines
    rawLines = fmap (lines) (readFile filePath)

parseRow :: String -> Row
parseRow string = [(digitToInt(value), False) | value <- string]


-- Main method
mainMethod :: Grid -> Int
mainMethod grid = countVisible finalGrid
  where
    finalGrid = (iterate checkNRotate grid) !! 4


countVisible :: Grid -> Int
countVisible grid = length [value | value <- concatGrid, snd value == True]
  where
    concatGrid = concat grid


checkNRotate :: Grid -> Grid
checkNRotate grid = rotate (checkRows grid)
  where
    rotate grid' = (transpose.reverse) grid'


checkRows :: Grid -> Grid
checkRows [] = []
checkRows (gridHead:gridTail) = checkRow (-1) gridHead : checkRows gridTail


checkRow :: Int -> Row -> Row
checkRow largest [] = []
checkRow largest (rowHead:rowTail) = snd setTreeTuple : checkRow (fst setTreeTuple) rowTail
  where
    setTreeTuple = setTree largest rowHead


setTree :: Int -> Tree -> (Int, Tree)
setTree largest (value, bool) | isLarger = (value, (value, True))
                              | otherwise = (largest, (value, bool))
  where
    isLarger = value > largest