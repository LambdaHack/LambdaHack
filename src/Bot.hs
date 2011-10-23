module Main where

import qualified System.Random as R
import System.Environment
import Control.Monad

move :: R.StdGen -> Int -> IO ()
move g k = do
  let (c, ng) = R.randomR ('0', 'z') g
  putChar c
  when (k > 0) $ move ng (k - 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [seed, count] ->
      -- Note that the seed is separate from the game seed.
      move (R.mkStdGen (read seed)) (read count)
    _ -> error "Two integer arguments required: random seed and iteration count."
