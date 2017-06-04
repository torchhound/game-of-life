module Main where

import Control.Concurrent
import Control.Monad.Random

data LifePosition = LifePosition {lpx :: Int, lpy :: Int} deriving (Show)

toss :: RandomGen g => Rand g Int
toss = getRandomR (0, 1)

newGame :: RandomGen g => Int -> Int -> Rand g [[Int]]
newGame x y = board
  where
    board = sequence (replicate y $ (sequence $ replicate x toss))

replaceXY :: Int -> Int -> Int -> [[Int]] -> [[Int]]
replaceXY x y new board =
  let row = board!!y
      (rhd, rtl) = splitAt x row
      newRow = rhd ++ (new:(tail rtl))
      (mhd, mtl) = splitAt y board
  in mhd ++ (newRow:(tail mtl))

getXY :: Int -> Int -> [[Int]] -> Int
getXY x y board =
  let row = board!!y
  in row!!x

lifeTick :: [[Int]] -> [[Int]]
lifeTick board = board

lifeLoop :: [[Int]] -> IO ()
lifeLoop board = do
  mapM print board
  putStr $ "\n"
  threadDelay 1000000
  let newBoard = lifeTick board
  lifeLoop newBoard

main :: IO ()
main = do
  gen <- getStdGen
  newBoard <- evalRandIO $ newGame 20 20
  lifeLoop newBoard