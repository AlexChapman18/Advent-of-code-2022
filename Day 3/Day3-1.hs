import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type Compartment = String
type Bag = (Compartment, Compartment)

-- Reads in the file
intakeFile :: FilePath -> IO Int
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod) bags
    bags = fmap (map lineToBag) rawLines
    rawLines = fmap (lines) (readFile filePath)

-- Converts the strings to ints
lineToBag :: String -> Bag
lineToBag line = (removeDupes (take lineLength line),removeDupes (drop lineLength line))
  where
    lineLength = (length line) `div` 2


-- Main method
mainMethod :: [Bag] -> Int
mainMethod bags = totalSum
  where
    totalSum = sum allCommonItemsValues
    allCommonItemsValues = map (listToValue) allCommonItems
    allCommonItems = map (commonItems) bags


removeDupes :: [Char] -> [Char]
removeDupes chars = map (head) (group (sort chars))



commonItems :: Bag -> [Char]
commonItems bag = traceShow (intersect (fst bag) (snd bag)) intersect (fst bag) (snd bag)

listToValue :: [Char] -> Int
listToValue chars = sum (map (charToValue) chars)

charToValue :: Char -> Int
charToValue char | isUpper char = (ord char) - 38
                 | otherwise = (ord char) - 96