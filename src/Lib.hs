module Lib
  ( run,
  )
where

import Data.List (unfoldr)
import System.Environment (getArgs)
import System.Random (mkStdGen, uniformR)
import Text.Read (readMaybe)

data Vector = Vector Int Int
  deriving (Show)

data Action = North | South | East | West | Random | Stay | PickUp
  deriving (Show)

data CellState = Empty | Can | Wall
  deriving (Show)

-- Situation for north, south, east, west and current cells respectively
data Situation = Situation CellState CellState CellState CellState CellState
  deriving (Show)

-- Height, width, threshold
data Env = Env Int Int Int
  deriving (Show)

type RobbyState = Vector

type Map = [Bool]

randomMap :: Int -> Map
randomMap p = map (<= p) randomArray
  where
    randomArray = rolls pureGen
    rolls = unfoldr $ Just . uniformR (1, 100)
    pureGen = mkStdGen 150

intToAction :: Int -> Action
intToAction 0 = North
intToAction 1 = South
intToAction 2 = East
intToAction 3 = West
intToAction 4 = Random
intToAction 5 = Stay
intToAction 6 = PickUp
intToAction _ = error "Invalid int for action"

randomActions :: [Action]
randomActions = map intToAction $ rolls pureGen
  where
    rolls = unfoldr $ Just . uniformR (1, 7)
    pureGen = mkStdGen 100

getAction :: Int -> [Action] -> Action
getAction _ [] = error "Out of bounds"
getAction 0 (x : xs) = x
getAction n (x : xs) = getAction (n -1) xs

cellStateToInt :: CellState -> Int
cellStateToInt Empty = 0
cellStateToInt Can = 1
cellStateToInt Wall = 2

getSituationIndex :: Situation -> Int
getSituationIndex (Situation n s e w c) =
  cellStateToInt n * 3 ^ 4
    + cellStateToInt s * 3 ^ 3
    + cellStateToInt e * 3 ^ 2
    + cellStateToInt w * 3
    + cellStateToInt c

pickUpCan :: Int -> Map -> Map
pickUpCan i [] = []
pickUpCan 0 (x : xs) = False : xs
pickUpCan i (x : xs) = x : pickUpCan (i - 1) xs

moveRobby :: Action -> RobbyState -> RobbyState
moveRobby North (Vector x y) = Vector (x + 1) y
moveRobby South (Vector x y) = Vector (x - 1) y
moveRobby East (Vector x y) = Vector x (y + 1)
moveRobby West (Vector x y) = Vector x (y - 1)
moveRobby _ (Vector x y) = Vector x y

parseArgs :: [String] -> Env
parseArgs (height : width : threshold : xs) = case rEnv of
  (Just h, Just w, Just t) -> Env h w t
  _ -> error "Invalid arguments"
  where
    rEnv = (readMaybe height, readMaybe width, readMaybe threshold)
parseArgs _ = error "Invalid arguments"

cli :: IO Env
cli = do
  parseArgs <$> getArgs

run :: IO ()
run = do
  args <- cli
  print args
