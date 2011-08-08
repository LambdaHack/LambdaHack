module Main where

import qualified System.Random as R
import System.Environment

move :: R.StdGen -> IO ()
move g = do
  let (c, ng) = R.randomR ('A', 'z') g
  putChar c
  move ng

main :: IO ()
main = do
  args <- getArgs
  let seed = read (head args)
  move (R.mkStdGen seed)
