-- | The main source code file of LambdaHack the game.
-- Module "TieKnot" is separated to make it usable in tests.
module Main
  ( main
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Options.Applicative as OA
import Control.Concurrent.Async
import qualified Control.Exception as Ex
import System.Exit

import Game.LambdaHack.Server (serverOptionsPI)
import TieKnot

-- | Tie the LambdaHack engine client, server and frontend code
-- with the game-specific content definitions, and run the game.
main :: IO ()
main = do
  serverOptions <- OA.execParser serverOptionsPI
  -- Avoid the bound thread that would slow down the communication.
  a <- async $ tieKnot serverOptions
  ex <- waitCatch a
  case ex of
    Right () -> return ()
    Left e -> case Ex.fromException e of
      Just ExitSuccess ->
        exitSuccess  -- we are in the main thread, so it really exits
      _ -> Ex.throwIO e
