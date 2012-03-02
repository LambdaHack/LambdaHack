-- | Queues implemented with two stacks to ensure fast writes.
module Game.LambdaHack.Utils.LQueue
  ( LQueue, newLQueue, nullLQueue, trimLQueue, tryReadLQueue, writeLQueue
  ) where

-- | Queues implemented with two stacks.
type LQueue a = ([a], [a])  -- (read_end, write_end)

-- | Create a new empty mutable queue.
newLQueue :: LQueue a
newLQueue = ([], [])

-- | Check if the queue is empty.
nullLQueue :: LQueue a -> Bool
nullLQueue (rs, ws) = null rs && null ws

-- | Remove all but the last written element of the queue.
trimLQueue :: LQueue a -> LQueue a
trimLQueue (_, w:_) = ([w], [])
trimLQueue ([], []) = ([], [])
trimLQueue (rs, []) = ([last rs], [])

-- | Try reading a queue. Return @Nothing@ if empty. Waits on the lock.
tryReadLQueue :: LQueue a -> Maybe (a, LQueue a)
tryReadLQueue (r : rs, ws) = Just (r, (rs, ws))
tryReadLQueue ([], []) = Nothing
tryReadLQueue ([], ws) = tryReadLQueue (reverse ws, [])

-- | Write to the queue. Faster than reading.
writeLQueue :: LQueue a -> a -> LQueue a
writeLQueue (rs, ws) w = (rs, w : ws)
