import System.IO
import Control.Monad
import Data.List
import Debug.Trace

-- Types
type Game = (Char, Char)
type Games = [Game]

-- Reads in the file
intakeFile :: FilePath -> IO Int
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod) gameLines
    gameLines = fmap (map stringToGame) rawLines
    rawLines = fmap (lines) (readFile filePath)

-- Converts the strings to ints
stringToGame :: String -> Game
stringToGame line = ((line!!0) , (line!!2))




-- Main method
mainMethod :: Games -> Int
mainMethod games = totalScore
  where
    totalScore = totalPlayValue + totalOutcomeScore
    totalPlayValue = sum (map (getPlayValue) games')
    totalOutcomeScore = sum (map (calcGameScore) games')
    games' = map (refactorGame) games


--Changes a game into its actual game
refactorGame :: Game -> Game
refactorGame (them, me) = (them, getPlay whatToDo)
  where
    whatToDo = [('A', 'X', 'Z'), ('A', 'Y', 'X'), ('A', 'Z', 'Y'),
                ('B', 'X', 'X'), ('B', 'Y', 'Y'), ('B', 'Z', 'Z'),
                ('C', 'X', 'Y'), ('C', 'Y', 'Z'), ('C', 'Z', 'X')]
    getPlay ((a,b,c):rest) | (a == them && b == me) = c
                           | otherwise = getPlay rest


--Gets the value of an individual play i.e. X Y or Z
getPlayValue :: Game -> Int
getPlayValue (them, me) | me == 'X' = 1
                        | me == 'Y' = 2
                        | me == 'Z' = 3

--Gets the score of the game, win, draw, loss
calcGameScore :: Game -> Int
calcGameScore (them, me) | win = 6
                         | draw = 3
                         | loss = 0
  where
    win = or [(isPlay 'C' 'X'), (isPlay 'A' 'Y'), (isPlay 'B' 'Z')]
    draw = or [(isPlay 'A' 'X'), (isPlay 'B' 'Y'), (isPlay 'C' 'Z')]
    loss = or [(isPlay 'B' 'X'), (isPlay 'C' 'Y'), (isPlay 'A' 'Z')]
    isPlay a b = and [them == a, me == b]
