import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type Total = Int
type Instruction = [Int]
type InstructionTotals = [Int]


----- parsing input
intakeFile :: FilePath -> IO [Char]
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod 0) flatCommands
    flatCommands = fmap (concat) commands
    commands = fmap (parseCommands 1) initalNoop
    initalNoop = fmap ("noop" :) rawLines
    rawLines = fmap (lines) (readFile filePath)


parseCommands :: Int -> [String] -> [Instruction]
parseCommands _ [] = []
parseCommands total (lineHead:lineTail) = command : parseCommands newTotal lineTail
  where
    newTotal = last command
    command = parseCommand total lineHead


parseCommand :: Total -> String -> InstructionTotals
parseCommand total "noop" = [total]
parseCommand total string = [total, total + value]
  where
    value = stringToInt stringValue
    stringValue = (words string) !! 1


stringToInt :: String -> Int
stringToInt value = read value :: Int


-- Main method
mainMethod :: Int -> InstructionTotals -> [Char]
mainMethod _ (_:[]) = []
mainMethod 40 instructionTotals = '|' : mainMethod 0 instructionTotals
mainMethod counter (instructHead:instructTail) = getChar : mainMethod (counter+1) instructTail
  where
    getChar | isVisible = '#'
            | otherwise = '.'
    isVisible = or [(counter == instructHead), (counter == instructHead+1), (counter == instructHead-1)]



