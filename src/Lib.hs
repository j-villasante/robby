{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.Loops (iterateM_)
import Control.Monad.State (State, evalState, execState, modify, state)
import Data.List (unfoldr)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Random (mkStdGen, uniformR)
import Text.Read (readMaybe)

data Vector = Vector Int Int
  deriving (Eq, Show)

data Action = North | South | East | West | Random | Stay | PickUp
  deriving (Show)

data CellState = Empty | Can | Wall
  deriving (Show)

-- Situation for north, south, east, west and current cells respectively
data Situation = Situation CellState CellState CellState CellState CellState
  deriving (Show)

-- Height, width
data Env = Env Int Int
  deriving (Eq, Show)

type RobbyPosition = Vector

type Map = [Bool]

data GameState = GameState Map RobbyPosition Env
  deriving (Eq, Show)

type GameOutput = Action

randomMap :: Map
randomMap = map (<= 10) randomArray
  where
    randomArray :: [Int]
    randomArray = rolls pureGen
    rolls = unfoldr $ Just . uniformR (1, 100)
    pureGen = mkStdGen 60

randomActions :: [Action]
randomActions = map intToAction $ rolls pureGen
  where
    rolls = unfoldr $ Just . uniformR (1, 7)
    pureGen = mkStdGen 100
    intToAction :: Int -> Action
    intToAction 1 = North
    intToAction 2 = South
    intToAction 3 = East
    intToAction 4 = West
    intToAction 5 = Random
    intToAction 6 = Stay
    intToAction 7 = PickUp
    intToAction _ = error "Invalid int for action"

getAction :: Situation -> [Action] -> Action
getAction situation actions =
  let index = situationToIndex situation
   in getActionByIndex index actions
  where
    getActionByIndex :: Int -> [Action] -> Action
    getActionByIndex _ [] = error "Out of bounds"
    getActionByIndex 0 (x : xs) = x
    getActionByIndex n (x : xs) = getActionByIndex (n -1) xs
    situationToIndex :: Situation -> Int
    situationToIndex (Situation n s e w c) =
      cellStateToInt n * 3 ^ 4
        + cellStateToInt s * 3 ^ 3
        + cellStateToInt e * 3 ^ 2
        + cellStateToInt w * 3
        + cellStateToInt c
    cellStateToInt :: CellState -> Int
    cellStateToInt Empty = 0
    cellStateToInt Can = 1
    cellStateToInt Wall = 2

getCellStateFromMap :: Maybe Int -> Map -> CellState
getCellStateFromMap Nothing map = Wall
getCellStateFromMap (Just i) [] = Wall
getCellStateFromMap (Just 0) (True : xs) = Can
getCellStateFromMap (Just 0) (False : xs) = Empty
getCellStateFromMap (Just i) (x : xs) = getCellStateFromMap (Just (i - 1)) xs

vectorToIndex :: Env -> Vector -> Maybe Int
vectorToIndex (Env height width) (Vector x y)
  | x < 0 || y < 0 = Nothing
  | i > height * width = Nothing
  | otherwise = Just i
  where
    i = x + y * width

moveRobby :: Env -> Action -> RobbyPosition -> RobbyPosition
moveRobby (Env height _) North (Vector x y)
  | y + 1 > height = Vector x y
  | otherwise = Vector x (y + 1)
moveRobby env South (Vector x y)
  | y - 1 < 0 = Vector x y
  | otherwise = Vector x (y - 1)
moveRobby (Env _ width) East (Vector x y)
  | x + 1 > width = Vector x y
  | otherwise = Vector (x + 1) y
moveRobby env West (Vector x y)
  | x - 1 < 0 = Vector x y
  | otherwise = Vector (x - 1) y
moveRobby _ _ (Vector x y) = Vector x y

transitionMap :: Env -> RobbyPosition -> Action -> Map -> Map
transitionMap (Env h w) (Vector x y) PickUp map = pickUpCan (x + y * w) map
  where
    pickUpCan :: Int -> Map -> Map
    pickUpCan i [] = []
    pickUpCan 0 (x : xs) = False : xs
    pickUpCan i (x : xs) = x : pickUpCan (i - 1) xs
transitionMap _ _ _ map = map

transition :: State GameState GameOutput
transition = state transformer
  where
    transformer :: GameState -> (GameOutput, GameState)
    transformer (GameState map (Vector x y) env) =
      let ni = vectorToIndex env (Vector x (y + 1))
          si = vectorToIndex env (Vector x (y - 1))
          ei = vectorToIndex env (Vector (x - 1) y)
          wi = vectorToIndex env (Vector (x + 1) y)
          ci = vectorToIndex env (Vector x y)
          ncs = getCellStateFromMap ni map
          scs = getCellStateFromMap si map
          ecs = getCellStateFromMap ei map
          wcs = getCellStateFromMap wi map
          ccs = getCellStateFromMap ci map
          action = getAction (Situation ncs scs ecs wcs ccs) randomActions
          nextPosition = moveRobby env action (Vector x y)
          nextMap = transitionMap env (Vector x y) action map
       in (action, GameState nextMap nextPosition env)

getInitPos :: Env -> RobbyPosition
getInitPos (Env x y) = Vector (x `div` 3) (y `div` 3)

displayWorld :: GameState -> T.Text
displayWorld (GameState _map (Vector x y) (Env height width)) = header (width + 2) <> finalMap zippedMap <> header (width + 2) <> "\n"
  where
    header :: Int -> T.Text
    header 0 = ""
    header i = "=" <> header (i - 1)
    boolToStr :: Bool -> T.Text
    boolToStr True = "c"
    boolToStr False = " "
    zippedMap :: [(Int, Bool)]
    zippedMap = zip [0 ..] _map
    finalMap :: [(Int, Bool)] -> T.Text
    finalMap ((i, val) : xs)
      | i == height * width = "|\n"
      | i == 0 = "\n|" <> boolToStr val <> finalMap xs
      | i `mod` width == 0 && i /= 0 = "|\n|" <> boolToStr val <> finalMap xs
      | i == y * width + x && val = "!" <> finalMap xs
      | i == y * width + x && not val = "o" <> finalMap xs
      | otherwise = boolToStr val <> finalMap xs
    finalMap [] = ""

parseArgs :: [String] -> Env
parseArgs (height : width : xs) = case rEnv of
  (Just h, Just w) -> Env h w
  _ -> error "Invalid arguments"
  where
    rEnv = (readMaybe height, readMaybe width)
parseArgs _ = error "Invalid arguments"

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

gameLoop :: GameState -> IO ()
gameLoop (GameState map pos env) = do
  clearScreen
  T.putStr $ displayWorld (GameState map pos env)
  threadDelay (1 * 1000000)
  gameLoop $ execState transition $ GameState map pos env

cli :: IO Env
cli =
  parseArgs <$> getArgs

run :: IO ()
run = do
  args <- cli
  gameLoop $ GameState randomMap (getInitPos args) args
