{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
-- | Game time and speed.
module Game.LambdaHack.Common.Time
  ( Time, timeTicks
  , timeZero, timeEpsilon, timeClip, timeTurn, timeSecond, clipsInTurn
  , absoluteTimeAdd, absoluteTimeSubtract, absoluteTimeNegate
  , timeFit, timeFitUp, timeRecent5, timeRecent20
  , Delta(..), timeShift, timeDeltaToFrom, timeDeltaAdd, timeDeltaSubtract
  , timeDeltaReverse, timeDeltaScale, timeDeltaPercent, timeDeltaDiv
  , timeDeltaToDigit, timeDeltaInSecondsText
  , Speed, toSpeed, fromSpeed, minSpeed, displaySpeed
  , speedZero, speedWalk, speedLimp, speedThrust, modifyDamageBySpeed
  , speedScale, speedAdd
  , ticksPerMeter, speedFromWeight, rangeFromSpeedAndLinger
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , _timeTick, turnsInSecond, sInMs, minimalSpeed, rangeFromSpeed
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.Char as Char
import           Data.Int (Int64)

import Game.LambdaHack.Common.Misc

-- | Game time in ticks. The time dimension.
-- One tick is 1 microsecond (one millionth of a second),
-- one turn is 0.5 s.
newtype Time = Time {timeTicks :: Int64}
  deriving (Show, Eq, Ord, Binary)

-- | Start of the game time, or zero lenght time interval.
timeZero :: Time
timeZero = Time 0

-- | The smallest unit of time. Should not be exported and used elsewhere,
-- because the proportion of turn to tick is an implementation detail.
-- The significance of this detail is only that it determines resolution
-- of the time dimension.
_timeTick :: Time
_timeTick = Time 1

-- | An infinitesimal time period.
timeEpsilon :: Time
timeEpsilon = _timeTick

-- | At least once per clip all moves are resolved
-- and a frame or a frame delay is generated.
-- Currently one clip is 0.05 s, but it may change,
-- and the code should not depend on this fixed value.
timeClip :: Time
timeClip = Time 50000

-- | One turn is 0.5 s. The code may depend on that.
-- Actors at normal speed (2 m/s) take one turn to move one tile (1 m by 1 m).
timeTurn :: Time
timeTurn = Time 500000

-- | This many ticks fits in a single second. Do not export,
timeSecond :: Time
timeSecond = Time $ timeTicks timeTurn * turnsInSecond

-- | This many turns fit in a single second.
turnsInSecond :: Int64
turnsInSecond = 2

-- | This many clips fit in one turn. Determines the resolution
-- of actor move sampling and display updates.
clipsInTurn :: Int
clipsInTurn =
  let r = timeTurn `timeFit` timeClip
  in assert (r >= 5) r

-- | Absolute time addition, e.g., for summing the total game session time
-- from the times of individual games.
absoluteTimeAdd :: Time -> Time -> Time
{-# INLINE absoluteTimeAdd #-}
absoluteTimeAdd (Time t1) (Time t2) = Time (t1 + t2)

absoluteTimeSubtract :: Time -> Time -> Time
{-# INLINE absoluteTimeSubtract #-}
absoluteTimeSubtract (Time t1) (Time t2) = Time (t1 - t2)

-- | Absolute time negation. To be used for reversing time flow,
-- e.g., for comparing absolute times in the reverse order.
absoluteTimeNegate :: Time -> Time
{-# INLINE absoluteTimeNegate #-}
absoluteTimeNegate (Time t) = Time (-t)

-- | How many time intervals of the latter kind fits in an interval
-- of the former kind.
timeFit :: Time -> Time -> Int
{-# INLINE timeFit #-}
timeFit (Time t1) (Time t2) = fromEnum $ t1 `div` t2

-- | How many time intervals of the latter kind cover an interval
-- of the former kind (rounded up).
timeFitUp :: Time -> Time -> Int
{-# INLINE timeFitUp #-}
timeFitUp (Time t1) (Time t2) = fromEnum $ t1 `divUp` t2

timeRecent5 :: Time -> Time -> Bool
timeRecent5 localTime time = timeDeltaToFrom localTime time
                             < timeDeltaScale (Delta timeTurn) 5

timeRecent20 :: Time -> Time -> Bool
timeRecent20 localTime time = timeDeltaToFrom localTime time
                              < timeDeltaScale (Delta timeTurn) 20

-- | One-dimentional vectors. Introduced to tell apart the 2 uses of Time:
-- as an absolute game time and as an increment.
newtype Delta a = Delta a
  deriving (Show, Eq, Ord, Binary, Functor)

-- | Shifting an absolute time by a time vector.
timeShift :: Time -> Delta Time -> Time
{-# INLINE timeShift #-}
timeShift (Time t1) (Delta (Time t2)) = Time (t1 + t2)

-- | Time time vector between the second and the first absolute times.
-- The arguments are in the same order as in the underlying scalar subtraction.
timeDeltaToFrom :: Time -> Time -> Delta Time
{-# INLINE timeDeltaToFrom #-}
timeDeltaToFrom (Time t1) (Time t2) = Delta $ Time (t1 - t2)

-- | Addition of time deltas.
timeDeltaAdd :: Delta Time -> Delta Time -> Delta Time
{-# INLINE timeDeltaAdd #-}
timeDeltaAdd (Delta (Time t1)) (Delta (Time t2)) = Delta $ Time (t1 - t2)

-- | Subtraction of time deltas.
-- The arguments are in the same order as in the underlying scalar subtraction.
timeDeltaSubtract :: Delta Time -> Delta Time -> Delta Time
{-# INLINE timeDeltaSubtract #-}
timeDeltaSubtract (Delta (Time t1)) (Delta (Time t2)) = Delta $ Time (t1 - t2)

-- | Reverse a time vector.
timeDeltaReverse :: Delta Time -> Delta Time
{-# INLINE timeDeltaReverse #-}
timeDeltaReverse (Delta (Time t)) = Delta (Time (-t))

-- | Scale the time vector by an @Int@ scalar value.
timeDeltaScale :: Delta Time -> Int -> Delta Time
{-# INLINE timeDeltaScale #-}
timeDeltaScale (Delta (Time t)) s = Delta (Time (t * fromIntegral s))

-- | Take the given percent of the time vector.
timeDeltaPercent :: Delta Time -> Int -> Delta Time
{-# INLINE timeDeltaPercent #-}
timeDeltaPercent (Delta (Time t)) s =
  Delta (Time (t * fromIntegral s `div` 100))

-- | Divide a time vector.
timeDeltaDiv :: Delta Time -> Int -> Delta Time
{-# INLINE timeDeltaDiv #-}
timeDeltaDiv (Delta (Time t)) n = Delta (Time (t `div` fromIntegral n))

-- | Represent the main 10 thresholds of a time range by digits,
-- given the total length of the time range.
timeDeltaToDigit :: Delta Time -> Delta Time -> Char
{-# INLINE timeDeltaToDigit #-}
timeDeltaToDigit (Delta (Time maxT)) (Delta (Time t)) =
  let n = (20 * t) `div` maxT
      k = (n + 1) `div` 2
      digit | k > 9     = '9'
            | k < 1     = '1'
            | otherwise = Char.intToDigit $ fromEnum k
  in digit

-- @oneM@ times the number of seconds represented by the time delta
timeDeltaInSeconds :: Delta Time -> Int64
timeDeltaInSeconds (Delta (Time dt)) = oneM * dt `div` timeTicks timeSecond

timeDeltaInSecondsText :: Delta Time -> Text
timeDeltaInSecondsText delta = show64With2 (timeDeltaInSeconds delta) <> "s"

-- | Speed in meters per 1 million seconds (m/Ms).
-- Actors at normal speed (2 m/s) take one time turn (0.5 s)
-- to make one step (move one tile, which is 1 m by 1 m).
newtype Speed = Speed Int64
  deriving (Eq, Ord, Binary)

instance Show Speed where
  show s = show $ fromSpeed s

-- | Number of seconds in a mega-second.
sInMs :: Int64
sInMs = 1000000

-- | Constructor for content definitions.
toSpeed :: Int -> Speed
{-# INLINE toSpeed #-}
toSpeed s = Speed $ fromIntegral s * sInMs `div` 10

-- | Readable representation of speed in the format used in content definitions.
fromSpeed :: Speed -> Int
{-# INLINE fromSpeed #-}
fromSpeed (Speed s) = fromEnum $ s * 10 `div` sInMs

minSpeed :: Int
minSpeed = 5

-- | Pretty-print speed given in the format used in content definitions.
displaySpeed :: Int -> String
displaySpeed kRaw =
  let k = max minSpeed kRaw
      l = k `div` 10
      x = k - l * 10
  in show l
     <> (if x == 0 then "" else "." <> show x)
     <> "m/s"

-- | The minimal speed is half a meter (half a step across a tile)
-- per second (two standard turns, which the time span during which
-- projectile moves, unless it has modified linger value).
-- This is four times slower than standard human movement speed.
--
-- It needen't be lower, because @rangeFromSpeed@ gives 0 steps
-- with such speed, so the actor's trajectory is empty, so it drops down
-- at once. Twice that speed already moves a normal projectile one step
-- before it stops. It shouldn't be lower or a slow actor would incur
-- such a time debt for performing a single action that he'd be paralyzed
-- for many turns, e.g., leaving his dead body on the screen for very long.
minimalSpeed :: Int64
minimalSpeed =
  let Speed msp = toSpeed minSpeed
  in assert (msp == sInMs `div` 2) msp

-- | No movement possible at that speed.
speedZero :: Speed
speedZero = Speed 0

-- | Fast walk speed (2 m/s) that suffices to move one tile in one turn.
speedWalk :: Speed
speedWalk = Speed $ 2 * sInMs

-- | Limp speed (1 m/s) that suffices to move one tile in two turns.
-- This is the minimal speed for projectiles to fly just one space and drop.
speedLimp :: Speed
speedLimp = Speed sInMs

-- | Sword thrust speed (10 m/s). Base weapon damages, both melee and ranged,
-- are given assuming this speed and ranged damage is modified
-- accordingly when projectile speeds differ. Differences in melee
-- weapon swing speeds are captured in damage bonuses instead,
-- since many other factors influence total damage.
--
-- Billiard ball is 25 m/s, sword swing at the tip is 35 m/s,
-- medieval bow is 70 m/s, AK47 is 700 m/s.
speedThrust :: Speed
speedThrust = Speed $ 10 * sInMs

-- | Modify damage when projectiles is at a non-standard speed.
-- Energy and so damage is proportional to the square of speed,
-- hence the formula.
modifyDamageBySpeed :: Int64 -> Speed -> Int64
modifyDamageBySpeed dmg (Speed s) =
  let Speed sThrust = speedThrust
  in if s <= minimalSpeed
     then 0  -- needed mostly not to display useless ranged damage
     else round (fromIntegral dmg * fromIntegral s ^ (2 :: Int)
                 / fromIntegral sThrust ^ (2 :: Int) :: Double)
                    -- Double, because overflows Int64

-- | Scale speed by a scalar value.
speedScale :: Rational -> Speed -> Speed
{-# INLINE speedScale #-}
speedScale s (Speed v) = Speed (round $ fromIntegral v * s)

-- | Speed addition.
speedAdd :: Speed -> Speed -> Speed
{-# INLINE speedAdd #-}
speedAdd (Speed s1) (Speed s2) = Speed (s1 + s2)

-- | The number of time ticks it takes to walk 1 meter at the given speed.
ticksPerMeter :: Speed -> Delta Time
{-# INLINE ticksPerMeter #-}
ticksPerMeter (Speed v) =
  -- Prevent division by zero or infinite time taken for any action.
  Delta $ Time $ timeTicks timeSecond * sInMs `divUp` max minimalSpeed v

-- | Calculate projectile speed from item weight in grams
-- and velocity percent modifier.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Item-statistics>.
speedFromWeight :: Int -> Int -> Speed
speedFromWeight !weight !throwVelocity =
  let w = fromIntegral weight
      mpMs | w < 250 = sInMs * 20
           | w < 1500 = sInMs * 20 * 1250 `div` (w + 1000)
           | w < 10500 = sInMs * (11500 - w) `div` 1000
           | otherwise = minimalSpeed * 2  -- move one step and drop
      v = mpMs * fromIntegral throwVelocity `div` 100
      -- We round down to the nearest multiple of 2M (unless the speed
      -- is very low), to ensure both turns of flight cover the same distance
      -- and that the speed matches the distance traveled exactly.
      multiple2M = if v > 2 * sInMs
                   then 2 * sInMs * (v `div` (2 * sInMs))
                   else v
  in Speed $ max minimalSpeed multiple2M

-- | Calculate maximum range in meters of a projectile from its speed.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Item-statistics>.
-- With this formula, each projectile flies for at most 1 second,
-- that is 2 standard turns, and then drops to the ground.
rangeFromSpeed :: Speed -> Int
{-# INLINE rangeFromSpeed #-}
rangeFromSpeed (Speed v) = fromEnum $ v `div` sInMs

-- | Calculate maximum range taking into account the linger percentage.
rangeFromSpeedAndLinger :: Speed -> Int -> Int
rangeFromSpeedAndLinger !speed !throwLinger =
  let range = rangeFromSpeed speed
  in throwLinger * range `divUp` 100
