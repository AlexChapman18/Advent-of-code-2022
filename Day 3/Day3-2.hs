import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type Bag = String
type Group = [Bag]

-- Reads in the file
intakeFile :: FilePath -> IO Int
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod) bags
    bags = fmap (map lineToBag) rawLines
    rawLines = fmap (lines) (readFile filePath)

-- Converts the strings to ints
lineToBag :: Bag -> Bag
lineToBag bag = removeDupes bag


-- Main method
mainMethod :: [Bag] -> Int
mainMethod bags = totalSum
  where
    totalSum = sum allCommonItemsValues
    allCommonItemsValues = map (listToValue) allCommonItems
    allCommonGroupItems = map (commonGroupItem) allGroups
    commonGroupItem group = foldl (commonItems) (head group) (tail group)
    allGroups = makeGroups bags

-- Makes groups of 3 bags
makeGroups :: [Bag] -> [Group]
makeGroups [] = []
makeGroups bags = (take 3 bags) : makeGroups (drop 3 bags)

-- Removes duplicate items from bags
removeDupes :: Bag -> Bag
removeDupes chars = map (head) (group (sort chars))

-- Gets common items within a pair of bags
commonItems :: Bag -> Bag -> [Char]
commonItems bag1 bag2 = intersect bag1 bag2

-- Converts a list of chars into their respective values
listToValue :: [Char] -> Int
listToValue chars = sum (map (charToValue) chars)

-- Turns a char into its respective value
charToValue :: Char -> Int
charToValue char | isUpper char = (ord char) - 38
                 | otherwise = (ord char) - 96