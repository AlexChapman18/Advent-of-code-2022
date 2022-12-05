import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type Stack = [Char]
type Step = (Int, Int, Int)

--Initial values
s1 = "ZVTBJGR"
s2 = "LVRJ"
s3 = "FQZ"
s4 = "GQVFLNHZ"
s5 = "WMSCJTQR"
s6 = "FHCTWS"
s7 = "JNFVCZD"
s8 = "QFRWDZGL"
s9 = "PVWBJ"
initialStacks = [s1,s2,s3,s4,s5,s6,s7,s8,s9]


-- Reads in the file
intakeFile :: FilePath -> IO [Char]
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod initialStacks) steps
    steps = fmap (map lineToPair) rawLines
    rawLines = fmap (lines) (readFile filePath)

-- Converts the strings to a Step
lineToPair :: String -> Step
lineToPair line = (getInt 0, getInt 1, getInt 2)
  where
    getInt n = read (splitLine !! n) :: Int
    splitLine = words (map (nonDigit) line)
    nonDigit = (\x -> if isDigit x then x else ' ')


-- Main method
mainMethod :: [Stack] -> [Step] -> [Char]
mainMethod stacks [] = getTops stacks
mainMethod stacks (step:rest) = mainMethod (applyStep stacks step) rest

getTops :: [Stack] -> [Char]
getTops [] = ""
getTops ("":rest) = "" ++ getTops rest
getTops (stack:rest) = [(head stack)] ++ getTops rest



applyStep :: [Stack] -> Step -> [Stack]
applyStep stacks (a,b,c) = addedStack
  where
    addedStack = [if indx == c then (crates ++ stck) else stck  | (stck,indx) <- removedStack]
    removedStack = [if indx == b then ((drop a stck),indx) else (stck,indx)  | (stck,indx) <- zipped]
    crates = getCrates stacks (a,b,c)
    zipped = zip stacks indexes
    indexes = [1..length stacks]

getCrates :: [Stack] -> Step -> [Char]
getCrates stacks (a,b,c) = take a (stacks !! (b-1))