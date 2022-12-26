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
intakeFile :: FilePath -> IO Int
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod) flatCommands
    flatCommands = fmap (concat) commands
    commands = fmap (parseCommands 1) initalNoop
    initalNoop = fmap ("noop" :) rawLines
    rawLines = fmap (lines) (readFile filePath)


parseCommands :: Int -> [String] -> [Instruction]
parseCommands _ [] = []
parseCommands total (lineHead:lineTail) = command : parseCommands newTotal lineTail
  where
    newTotal = traceShow (command) last command
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
mainMethod :: InstructionTotals -> Int
mainMethod cycleTotals = traceShow (cycleTotals) foldl (sumFunction) 0 readCycles
  where
    sumFunction total cycle = traceShow (cycleTotals !! (cycle+2)) total + ((cycleTotals !! (cycle-1)) * cycle)

readCycles = [20,60,100,140,180,220]


