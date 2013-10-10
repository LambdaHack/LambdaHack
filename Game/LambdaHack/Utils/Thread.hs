-- | Keeping track of forked threads.
module Game.LambdaHack.Utils.Thread
  ( forkFinally, forkChild, waitForChildren
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, mask, try)
import System.IO.Unsafe (unsafePerformIO)

-- Swiped from http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html

-- Already there in GHC >= 7.6, but we can use older GHCs.
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

-- Warning: this is a totally global variable, spanning all threads, etc.
children :: MVar [MVar ()]
{-# NOINLINE children #-}
children = unsafePerformIO (newMVar [])

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar : childs)
  forkFinally io (\_ -> putMVar mvar ())

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    [] -> return ()
    m : ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren
