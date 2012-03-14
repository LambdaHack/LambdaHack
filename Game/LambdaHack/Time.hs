-- | Game time and speed.
module Game.LambdaHack.Time
  ( Time, timeZero, timeTurn, timeStep  -- do not add timeTick!
  , timeAdd, timeFit, timeNegate, timeScale
  , timeToDigit
  , Speed, toSpeed, speedNormal
  , speedScale, ticksPerMeter, traveled, speedFromWeight, rangeFromSpeed
  ) where

import Data.Binary
import qualified Data.Char as Char

-- | Game time in ticks. The time dimension.
-- One tick is 1 microsecond (one millionth of a second), one turn is 0.1 s,
-- one step is 0.5 s. Moves are resolved and screen frame is generated
-- at least every turn.
newtype Time = Time Word64
  deriving (Show, Eq, Ord)

instance Binary Time where
  put (Time n) = put n
  get = fmap Time get

-- | Start of the game time, or zero lenght time interval.
timeZero :: Time
timeZero = Time 0

-- | The smallest unit of time. It is not exported, because the proportion
-- of step or turn to tick is an implementation detail.
-- The significance of this detail is only that it determines resolution
-- of the time dimension.
_timeTick :: Time
_timeTick = Time 1

-- | At least once per turn all moves are resolved and a frame
-- or a frame delay is generated.
-- Currently one turn is 0.1 s, but it may change,
-- and the code should not depend on this fixed value.
timeTurn :: Time
timeTurn = Time 100000

-- | One step is 0.5 s. The code may depend on that.
-- Actors at normal speed (2 m/s) take one step to move one tile (1 m by 1 m).
timeStep :: Time
timeStep = Time 500000

-- | This many steps fits in a single second.
stepsInSecond :: Word64
stepsInSecond = 2

-- | This many ticks fits in a single second.
ticksInSecond :: Word64
ticksInSecond =
  let Time ticksInStep = timeStep
  in ticksInStep * stepsInSecond

-- | Time addition.
timeAdd :: Time -> Time -> Time
timeAdd (Time t1) (Time t2) = Time (t1 + t2)

-- | How many time intervals of the second kind fits in an interval
-- of the first kind.
timeFit :: Time -> Time -> Int
timeFit (Time t1) (Time t2) = fromIntegral $ t1 `div` t2

-- | Negate a time interval. Can be used to subtract from a time
-- or to reverse the ordering on time.
timeNegate :: Time -> Time
timeNegate (Time t) = Time (-t)

-- | Scale time by an Int scalar value.
timeScale :: Time -> Int -> Time
timeScale (Time t) s = Time (t * fromIntegral s)

-- | Represent the main 10 thresholds of a time range by digits,
-- given the total length of the time range.
timeToDigit :: Time -> Time -> Char
timeToDigit (Time maxT) (Time t) =
  let k = 10 * t `div` maxT
      digit | k > 9     = '*'
            | k < 0     = '-'
            | otherwise = Char.intToDigit $ fromIntegral k
  in digit

-- | Speed in meters per 1 million seconds (m/Ms).
-- Actors at normal speed (2 m/s) take one time step (0.5 s)
-- to move one tile (1 m by 1 m).
newtype Speed = Speed Word64
  deriving (Show, Eq, Ord)

instance Binary Speed where
  put (Speed n) = put n
  get = fmap Speed get

-- | Number of seconds in a kilo-second.
sInMs :: Word64
sInMs = 1000000

-- | Constructor for content definitions.
toSpeed :: Double -> Speed
toSpeed s = Speed $ round $ s * fromIntegral sInMs

-- | Normal speed (2 m/s) that suffices to move one tile in one step.
speedNormal :: Speed
speedNormal = Speed $ 2 * sInMs

-- | Scale speed by an Int scalar value.
speedScale :: Speed -> Int -> Speed
speedScale (Speed v) s = Speed (v * fromIntegral s)

-- | The number of time ticks it takes to walk 1 meter at the given speed.
ticksPerMeter :: Speed -> Time
ticksPerMeter (Speed v) = Time $ ticksInSecond * sInMs `div` v

-- | Distance in meters (so also in tiles, given the chess metric)
-- traveled in a given time by a body with a given speed.
traveled :: Speed -> Time -> Int
traveled (Speed v) (Time t) =
  fromIntegral $ v * t `div` (ticksInSecond * sInMs)

-- | Calculate projectile speed from item weight in grams
-- and speed bonus in percents.
-- See https://github.com/kosmikus/LambdaHack/wiki/Item-statistics.
speedFromWeight :: Int -> Int -> Speed
speedFromWeight weight bonus =
  let w = fromIntegral weight
      b = fromIntegral bonus
      mpMs | w <= 500 = sInMs * 16
           | w > 500 && w <= 2000 = sInMs * 16 * 1500 `div` (w + 1000)
           | otherwise = sInMs * (10000 - w) `div` 1000
  in Speed $ max 0 $ mpMs * (100 + b) `div` 100

-- | Calculate maximum range in meters of a projectile from its speed.
-- See https://github.com/kosmikus/LambdaHack/wiki/Item-statistics.
rangeFromSpeed :: Speed -> Int
rangeFromSpeed (Speed v) = fromIntegral $ v `div` sInMs
