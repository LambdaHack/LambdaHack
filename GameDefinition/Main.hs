-- | The main source code file of LambdaHack the game.
module Main ( main ) where

import System.Environment (getArgs)

import TieKnot

-- | Tie the LambdaHack engine client, server and frontend code
-- with the game-specific content definitions, and run the game.
main :: IO ()
main = do
  args <- getArgs
  tieKnot args
