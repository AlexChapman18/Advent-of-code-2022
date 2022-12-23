import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char
import Data.Ord (comparing)
import Data.List (maximumBy)

-- Types
type View = Int
type Tree = (Int, View)
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
parseRow string = [(digitToInt(value), 1) | value <- string]


-- Main method
mainMethod :: Grid -> Int
mainMethod grid = findBest finalGrid
  where
    finalGrid = (iterate checkNRotate grid) !! 4


findBest :: Grid -> Int
findBest grid = snd (maximumBy (comparing snd) concatGrid)
  where
    concatGrid = concat grid


checkNRotate :: Grid -> Grid
checkNRotate grid = rotate (checkRows grid)
  where
    rotate grid' = (transpose.reverse) grid'


checkRows :: Grid -> Grid
checkRows grid = map checkRow grid


checkRow :: Row -> Row
checkRow [] = []
checkRow (rowHead:rowTail) = setTreeResult : checkRow rowTail
  where
    setTreeResult = setTree 0 rowHead rowTail


setTree :: Int -> Tree -> Row -> Tree
setTree 0 (height, score) [] = (height, 0)
setTree distance (height, score) [] = (height, distance * score)
setTree distance (height, score) ((headHeight, _):rowTail) | isTaller = (height, (distance+1) * score)
                                                           | otherwise = setTree (distance+1) (height, score) rowTail
  where
    isTaller = headHeight >= height