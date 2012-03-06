-- | Queues implemented with two stacks to ensure fast writes.
module Game.LambdaHack.Utils.LQueue
  ( LQueue, newLQueue, nullLQueue, trimLQueue, tryReadLQueue, writeLQueue
  ) where

import Data.Maybe

-- | Queues implemented with two stacks.
type LQueue a = ([a], [a])  -- (read_end, write_end)

-- | Create a new empty mutable queue.
newLQueue :: LQueue a
newLQueue = ([], [])

-- | Check if the queue is empty.
nullLQueue :: LQueue a -> Bool
nullLQueue (rs, ws) = null rs && null ws

-- | Remove all but the last written non-@Nothing@ element of the queue.
trimLQueue :: LQueue (Maybe a) -> LQueue (Maybe a)
trimLQueue (rs, ws) =
  let trim (_, w:_) = ([w], [])
      trim ([], []) = ([], [])
      trim (rsj, []) = ([last rsj], [])
  in trim (filter isJust rs, filter isJust ws)

-- | Try reading a queue. Return @Nothing@ if empty. Waits on the lock.
tryReadLQueue :: LQueue a -> Maybe (a, LQueue a)
tryReadLQueue (r : rs, ws) = Just (r, (rs, ws))
tryReadLQueue ([], []) = Nothing
tryReadLQueue ([], ws) = tryReadLQueue (reverse ws, [])

-- | Write to the queue. Faster than reading.
writeLQueue :: LQueue a -> a -> LQueue a
writeLQueue (rs, ws) w = (rs, w : ws)
