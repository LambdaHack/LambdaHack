-- | Keeping track of forked threads.
module Game.LambdaHack.Utils.Thread
  ( forkChild, waitForChildren
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar

-- Swiped from http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html. Ported to Async to link exceptions, to let travis tests fail.

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
    [] -> return ()
    m : ms -> do
      putMVar children ms
      wait m
      waitForChildren children
