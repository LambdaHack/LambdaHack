-- | Keeping track of forked threads.
module Game.LambdaHack.Utils.Thread
  ( forkChild, waitForChildren
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar
import Control.Exception (finally)

-- Swiped from http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html

forkChild :: MVar [MVar ()] -> IO () -> IO ThreadId
forkChild children io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar : childs)
  -- @forkFinally@ causes the program not to print client assertion failures
  -- forkFinally io (\_ -> putMVar mvar ())
  forkIO (io `finally` putMVar mvar ())

waitForChildren :: MVar [MVar ()] -> IO ()
waitForChildren children = do
  cs <- takeMVar children
  case cs of
    [] -> return ()
    m : ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren children
