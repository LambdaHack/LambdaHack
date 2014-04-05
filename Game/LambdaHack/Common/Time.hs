{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Game time and speed.
module Game.LambdaHack.Common.Time
  ( Time, timeZero, timeClip, timeTurn, timeEpsilon
  , absoluteTimeAdd, absoluteTimeNegate, timeFit, timeFitUp
  , Delta(..), timeShift, timeDeltaToFrom, timeDeltaReverse, timeDeltaScale
  , timeDeltaToDigit, ticksPerMeter
  , Speed, toSpeed, speedZero, speedNormal, speedScale, speedAdd, speedNegate
  , speedFromWeight, rangeFromSpeed
  ) where

import Data.Binary
import qualified Data.Char as Char
import Data.Int (Int64)

import Game.LambdaHack.Common.Misc

-- | Game time in ticks. The time dimension.
-- One tick is 1 microsecond (one millionth of a second),
-- one turn is 0.5 s.
newtype Time = Time Int64
  deriving (Show, Eq, Ord, Enum, Bounded, Binary)

-- | One-dimentional vectors. Introduced to tell apart the 2 uses of Time:
-- as an absolute game time and as an increment.
newtype Delta a = Delta a
  deriving (Show, Eq, Ord, Enum, Bounded, Binary)

instance Functor Delta where
  fmap f (Delta a) = Delta (f a)

-- | Start of the game time, or zero lenght time interval.
timeZero :: Time
timeZero = Time 0

-- | The smallest unit of time. Do not export, because the proportion
-- of turn to tick is an implementation detail.
-- The significance of this detail is only that it determines resolution
-- of the time dimension.
_timeTick :: Time
_timeTick = Time 1

-- | An infinitesimal time period.
timeEpsilon :: Time
timeEpsilon = _timeTick

-- TODO: don't have a fixed time, but instead set it at 1/3 or 1/4
-- of timeTurn depending on level. Clips are a UI feature
-- after all, so should depend on the user situation.
-- | At least once per clip all moves are resolved and a frame
-- or a frame delay is generated.
-- Currently one clip is 0.1 s, but it may change,
-- and the code should not depend on this fixed value.
timeClip :: Time
timeClip = Time 100000

-- | One turn is 0.5 s. The code may depend on that.
-- Actors at normal speed (2 m/s) take one turn to move one tile (1 m by 1 m).
timeTurn :: Time
timeTurn = Time 500000

-- | This many turns fit in a single second.
turnsInSecond :: Int64
turnsInSecond = 2

-- | This many ticks fits in a single second. Do not export,
_ticksInSecond :: Int64
_ticksInSecond =
  let Time ticksInTurn = timeTurn
  in ticksInTurn * turnsInSecond

-- | Absolute time addition, e.g., for summing the total game session time
-- from the times of individual games.
absoluteTimeAdd :: Time -> Time -> Time
absoluteTimeAdd (Time t1) (Time t2) = Time (t1 + t2)

-- | Shifting an absolute time by a time vector.
timeShift :: Time -> Delta Time -> Time
timeShift (Time t1) (Delta (Time t2)) = Time (t1 + t2)

-- | How many time intervals of the latter kind fits in an interval
-- of the former kind.
timeFit :: Time -> Time -> Int
timeFit (Time t1) (Time t2) = fromIntegral $ t1 `div` t2

-- | How many time intervals of the latter kind cover an interval
-- of the former kind (rounded up).
timeFitUp :: Time -> Time -> Int
timeFitUp (Time t1) (Time t2) = fromIntegral $ t1 `divUp` t2

-- | Reverse a time vector.
timeDeltaReverse :: Delta Time -> Delta Time
timeDeltaReverse (Delta (Time t)) = Delta (Time (-t))

-- | Absolute time negation. To be used for reversing time flow,
-- e.g., for comparing absolute times in the reverse order.
absoluteTimeNegate :: Time -> Time
absoluteTimeNegate (Time t) = Time (-t)

-- | Time time vector between the second and the first absolute times.
-- The arguments are in the same order as in the underlying scalar subtraction.
timeDeltaToFrom :: Time -> Time -> Delta Time
timeDeltaToFrom (Time t1) (Time t2) = Delta $ Time (t1 - t2)

-- | Scale the time vector by an @Int@ scalar value.
timeDeltaScale :: Delta Time -> Int -> Delta Time
timeDeltaScale (Delta (Time t)) s = Delta (Time (t * fromIntegral s))

-- | Represent the main 10 thresholds of a time range by digits,
-- given the total length of the time range.
timeDeltaToDigit :: Delta Time -> Delta Time -> Char
timeDeltaToDigit (Delta (Time maxT)) (Delta (Time t)) =
  let k = 10 * t `div` maxT
      digit | k > 9     = '*'
            | k < 0     = '-'
            | otherwise = Char.intToDigit $ fromIntegral k
  in digit

-- | Speed in meters per 1 million seconds (m/Ms).
-- Actors at normal speed (2 m/s) take one time turn (0.5 s)
-- to move one tile (1 m by 1 m).
newtype Speed = Speed Int64
  deriving (Show, Eq, Ord, Binary)

-- | Number of seconds in a mega-second.
sInMs :: Int64
sInMs = 1000000

-- | Constructor for content definitions.
toSpeed :: Double -> Speed
toSpeed s = Speed $ round $ s * fromIntegral sInMs

-- | No movement possible at that speed.
speedZero :: Speed
speedZero = Speed 0

-- | Normal speed (2 m/s) that suffices to move one tile in one turn.
speedNormal :: Speed
speedNormal = Speed $ 2 * sInMs

-- | Scale speed by an @Int@ scalar value.
speedScale :: Rational -> Speed -> Speed
speedScale s (Speed v) = Speed (round $ fromIntegral v * s)

-- | Speed addition.
speedAdd :: Speed -> Speed -> Speed
speedAdd (Speed s1) (Speed s2) = Speed (s1 + s2)

-- | Speed negation.
speedNegate :: Speed -> Speed
speedNegate (Speed n) = Speed (-n)

-- | The number of time ticks it takes to walk 1 meter at the given speed.
ticksPerMeter :: Speed -> Delta Time
ticksPerMeter (Speed v) = Delta $ Time $ _ticksInSecond * sInMs `divUp` v

-- | Calculate projectile speed from item weight in grams
-- and speed bonus in percents.
-- See <https://github.com/kosmikus/LambdaHack/wiki/Item-statistics>.
speedFromWeight :: Int -> Int -> Speed
speedFromWeight weight bonus =
  let w = fromIntegral weight
      b = fromIntegral bonus
      mpMs | w <= 500 = sInMs * 16
           | w > 500 && w <= 2000 = sInMs * 16 * 1500 `div` (w + 1000)
           | otherwise = sInMs * (10000 - w) `div` 1000
      v = mpMs * (100 + b) `div` 100
      -- We round down to the nearest multiple of 2M (unless the speed
      -- is very low), to ensure both turns of flight cover the same distance
      -- and that the speed matches the distance traveled exactly.
      multiple2M = sInMs * if v >= 2 * sInMs
                           then 2 * (v `div` (2 * sInMs))
                           else v `div` sInMs
  in Speed $ max 1 multiple2M

-- | Calculate maximum range in meters of a projectile from its speed.
-- See <https://github.com/kosmikus/LambdaHack/wiki/Item-statistics>.
-- With this formula, each projectile flies for at most 1 second,
-- that is 2 turns, and then drops to the ground.
rangeFromSpeed :: Speed -> Int
rangeFromSpeed (Speed v) = fromIntegral $ v `div` sInMs
