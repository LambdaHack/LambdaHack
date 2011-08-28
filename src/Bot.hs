module Main where

import qualified System.Random as R
import System.Environment
import Control.Monad

move :: R.StdGen -> Int -> IO ()
move g k = do
  let (c, ng) = R.randomR ('>', 'z') g
  putChar c
  when (k > 0) $ move ng (k - 1)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $
    error "Two integer arguments required: random seed and iteration count."
  let [seed, count] = args
  move (R.mkStdGen (read seed)) (read count)
