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

data Action = North | South | East | West

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

pickUpCan :: Int -> Map -> Map
pickUpCan i [] = []
pickUpCan 0 (x : xs) = False : xs
pickUpCan i (x : xs) = x : pickUpCan (i - 1) xs

moveRobby :: Action -> RobbyState -> RobbyState
moveRobby North (Vector x y) = Vector (x + 1) y
moveRobby South (Vector x y) = Vector (x - 1) y
moveRobby East (Vector x y) = Vector x (y + 1)
moveRobby West (Vector x y) = Vector x (y - 1)

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
