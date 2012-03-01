-- | Mutable queues protected by a single lock for reading and writing.
-- Cheaper and safer than a Chan, but with all threads waiting whenever
-- one is using the queue in any way.
-- .
-- The queue is implemented with two stacks to look very shortly for writes.
module Game.LambdaHack.Utils.LQueue
  ( LQueue, newLQueue, nullLQueue, clearLQueue, tryReadLQueue, writeLQueue
  ) where

import Control.Concurrent

-- | Mutable queues protected by a lock.
data LQueue a = LQueue (MVar ([a], [a]))  -- (read_end, write_end)

-- | Create a new empty mutable queue.
newLQueue :: IO (LQueue a)
newLQueue = fmap LQueue $ newMVar ([], [])

-- | Check if the queue is empty.
nullLQueue :: LQueue a -> IO Bool
nullLQueue (LQueue lq) = do
  (rs, ws) <- readMVar lq
  return $ null rs && null ws

-- | Empty an @LQueue@. Waits on the lock.
clearLQueue :: LQueue a -> IO ()
clearLQueue (LQueue lq) = do
  takeMVar lq
  putMVar lq ([], [])

-- | Try reading a queue. Return @Nothing@ if empty. Waits on the lock.
tryReadLQueue :: LQueue a -> IO (Maybe a)
tryReadLQueue (LQueue lq) = do
  qRaw <- takeMVar lq
  let update (r : rs, ws) = do
        putMVar lq (rs, ws)
        return $ Just r
      update ([], []) = do
        putMVar lq ([], [])
        return Nothing
      update ([], ws) =
        update (reverse ws, [])
  update qRaw

-- | Write to the queue. Faster than reading. Waits on the lock.
writeLQueue :: LQueue a -> a -> IO ()
writeLQueue (LQueue lq) w = do
  (rs, ws) <- takeMVar lq
  putMVar lq (rs, w : ws)
