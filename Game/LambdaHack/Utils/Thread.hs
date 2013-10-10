-- | Keeping track of forked threads.
module Game.LambdaHack.Utils.Thread
  ( forkFinally, forkChild, waitForChildren
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, mask, try)

-- Swiped from http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html

-- Already there in GHC >= 7.6, but we can use older GHCs.
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

forkChild :: MVar [MVar ()] -> IO () -> IO ThreadId
forkChild children io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar : childs)
  forkFinally io (\_ -> putMVar mvar ())

waitForChildren :: MVar [MVar ()] -> IO ()
waitForChildren children = do
  cs <- takeMVar children
  case cs of
    [] -> return ()
    m : ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren children
