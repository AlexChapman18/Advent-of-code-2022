import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type Vector = (Int, Int)
type Action = (Vector, Int)
type Pos = (Int, Int)
type Rope = [Pos]
type Direction = Char


-- Reads in the file
----- parsing input
intakeFile :: FilePath -> IO Int
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod) actions
    actions = fmap (map parseAction) rawLines
    rawLines = fmap (lines) (readFile filePath)

parseAction :: String -> Action
parseAction string = (vector, distance)
  where
    distance = stringToInt (splitString !! 1)
    vector = getVector direction
    direction = ((splitString !! 0) !! 0)
    splitString = words string

stringToInt :: String -> Int
stringToInt "" = -1
stringToInt value = read value :: Int

getVector :: Char -> Vector
getVector 'U' = (0,1)
getVector 'R' = (1,0)
getVector 'D' = (0,-1)
getVector  _  = (-1,0)

----- parsing input

-- Main method
mainMethod :: [Action] -> Int
mainMethod actions = number
  where
    number = countUnique positions
    positions = processActions emptyRope actions
    emptyRope = take 10 (repeat (0,0))


countUnique :: [[Pos]] -> Int
countUnique ropeTails = number
  where
    number = length unique
    unique = map (head) (group (sort flattened))
    flattened = concat ropeTails


processActions :: Rope -> [Action] -> [[Pos]]
processActions _ [] = []
processActions currentPositions (headAction:tailActions) = tailVisited : processActions lastVisited tailActions
  where
    tailVisited = map last allVisited
    lastVisited = last allVisited
    allVisited = currentPositions : processAction headAction currentPositions


processAction :: Action -> Rope -> [Rope]
processAction (vect, 0) rope = [rope]
processAction (vect, dis) rope = nextRope : processAction (vect, (dis-1)) nextRope
  where
    nextRope = getNextRope vect rope


getNextRope :: Vector -> Rope -> Rope
getNextRope vector [] = []
getNextRope vector (ropeHead:ropeTail) = scanl (getNextTail) (getNextHead vector ropeHead) ropeTail


getNextHead :: Vector -> Pos -> Pos
getNextHead (a, b) (x1, y1) = (a + x1, b + y1)


getNextTail :: Pos -> Pos -> Pos
getNextTail (x1, y1) (x2, y2) | not (shouldMove (x1, y1) (x2, y2)) = (x2, y2)
                              | otherwise = (x2 + xVect, y2 + yVect)
  where
    xVect = getVect (x1-x2)
    yVect = getVect (y1-y2)
    getVect value | value > 0 = 1
                  | value == 0 = 0
                  | otherwise = -1

shouldMove :: Pos -> Pos -> Bool
shouldMove (x1, y1) (x2, y2) = isNotClose
  where
    isNotClose = or [(absXDiff > 1), (absYDiff > 1)]
    absXDiff = abs (x1-x2)
    absYDiff = abs (y1-y2)