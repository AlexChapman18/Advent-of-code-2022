import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type Vector = (Int, Int)
type Action = (Vector, Int)
type Pos = (Int, Int)
type Positions = (Pos, Pos)
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
    positions = processActions ((0,0),(0,0)) actions

countUnique :: [[Positions]] -> Int
countUnique positions = number
  where
    number = length unique
    unique = map (head) (group (sort flattened))
    flattened = map snd $ concat positions


processActions :: Positions -> [Action] -> [[Positions]]
processActions _ [] = []
processActions currentPositions (headAction:tailActions) = allVisited : processActions lastVisited tailActions
  where
    lastVisited = last allVisited
    allVisited = currentPositions : processAction headAction currentPositions


processAction :: Action -> Positions -> [Positions]
processAction (vect, 0) ((x1,y1),(x2,y2)) = [((x1,y1),(x2,y2))]
processAction (vect, dis) positions = nextPositions : processAction (vect, (dis-1)) nextPositions
  where
    nextPositions = getNextPos vect positions

getNextPos :: Vector -> Positions -> Positions
getNextPos (a, b) ((x1, y1), (x2, y2)) = (getNextHead, (getNextTail (getNextHead, (x2, y2))))
  where
    getNextHead = (a + x1, b + y1)

getNextTail :: Positions -> Pos
getNextTail ((x1, y1), (x2, y2)) | not (shouldMove ((x1, y1), (x2, y2))) = (x2, y2)
                                 | otherwise = (x2 + xVect, y2 + yVect)
  where
    xVect = getVect (x1-x2)
    yVect = getVect (y1-y2)
    getVect value | value > 0 = 1
                  | value == 0 = 0
                  | otherwise = -1

shouldMove :: Positions -> Bool
shouldMove ((x1, y1), (x2, y2)) = isNotClose
  where
    isNotClose = or [(absXDiff > 1), (absYDiff > 1)]
    absXDiff = abs (x1-x2)
    absYDiff = abs (y1-y2)