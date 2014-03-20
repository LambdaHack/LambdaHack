-- | Queues implemented with two stacks to ensure fast writes.
module Game.LambdaHack.Common.LQueue
  ( LQueue
  , newLQueue, nullLQueue, lengthLQueue, tryReadLQueue, writeLQueue
  , trimLQueue, dropStartLQueue, lastLQueue, toListLQueue
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

-- | The length of the queue.
lengthLQueue :: LQueue a -> Int
lengthLQueue (rs, ws) = length rs + length ws

-- | Try reading a queue. Return @Nothing@ if empty.
tryReadLQueue :: LQueue a -> Maybe (a, LQueue a)
tryReadLQueue (r : rs, ws) = Just (r, (rs, ws))
tryReadLQueue ([], []) = Nothing
tryReadLQueue ([], ws) = tryReadLQueue (reverse ws, [])

-- | Write to the queue. Faster than reading.
writeLQueue :: LQueue a -> a -> LQueue a
writeLQueue (rs, ws) w = (rs, w : ws)

-- | Remove all but the last written non-@Nothing@ element of the queue.
trimLQueue :: LQueue (Maybe a) -> LQueue (Maybe a)
trimLQueue (rs, ws) =
  let trim (_, w:_) = ([w], [])
      trim ([], []) = ([], [])
      trim (rsj, []) = ([last rsj], [])
  in trim (filter isJust rs, filter isJust ws)

-- | Remove frames up to and including the first segment of @Nothing@ frames.
-- | If the resulting queue is empty, apply trimLQueue instead.
dropStartLQueue :: LQueue (Maybe a) -> LQueue (Maybe a)
dropStartLQueue (rs, ws) =
  let dq = (dropWhile isNothing $ dropWhile isJust $ rs ++ reverse ws, [])
  in if nullLQueue dq then trimLQueue (rs, ws) else dq

-- | Dump all but the last written non-@Nothing@ element of the queue, if any.
lastLQueue :: LQueue (Maybe a) -> Maybe a
lastLQueue (rs, ws) =
  let lst (_, w:_) = Just w
      lst ([], []) = Nothing
      lst (rsj, []) = Just $ last rsj
  in lst (catMaybes rs, catMaybes ws)

toListLQueue :: LQueue a -> [a]
toListLQueue (rs, ws) = rs ++ reverse ws
