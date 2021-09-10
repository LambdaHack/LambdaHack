-- | Keeping track of forked threads.
module Game.LambdaHack.Common.Thread
  ( forkChild, waitForChildren
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Control.Concurrent.Async
import Control.Concurrent.MVar

-- Swiped from <http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html>
-- Ported to Async to link exceptions, to let CI tests fail.

forkChild :: MVar [Async ()] -> IO () -> IO ()
forkChild children io = do
  a <- async io
  link a
  childs <- takeMVar children
  putMVar children (a : childs)

waitForChildren :: MVar [Async ()] -> IO ()
waitForChildren children = do
  cs <- takeMVar children
  case cs of
    [] -> do
      putMVar children []
      return ()
    m : ms -> do
      putMVar children ms
      wait m
      waitForChildren children
