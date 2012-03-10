-- | Hacks that haven't found their home yet.
module Game.LambdaHack.Misc
  ( normalLevelBound, Time, divUp, Freqs
  ) where

-- | Level bounds. TODO: query terminal size instead and scroll view.
normalLevelBound :: (Int, Int)
normalLevelBound = (79, 21)

-- | Game time in ticks. The time dimension.
-- One tick is 0.05s, one turn is 0.05s, one step is 0.5s.
-- Moves are resolved and screen frame is generated every turn.
-- Actors at normal speed (2 m/s) take one step to move one tile (1 m by 1 m).
type Time = Int

-- | Integer division, rounding up.
divUp :: Int -> Int -> Int
divUp n k = (n + k - 1) `div` k

-- | For each group that the kind belongs to, denoted by a @String@ name
-- in the first component of a pair, the second component of a pair shows
-- how common the kind is within the group.
type Freqs = [(String, Int)]
