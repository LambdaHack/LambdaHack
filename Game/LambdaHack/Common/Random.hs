{-# LANGUAGE DeriveGeneric, FlexibleInstances, TypeSynonymInstances #-}
-- | Representation of probabilities and random computations.
module Game.LambdaHack.Common.Random
  ( -- * The @Rng@ monad
    Rnd
    -- * Random operations
  , randomR, random, oneOf, frequency, cast
    -- * Casting dice
  , RollDice, rollDice, castDice, maxDice, minDice, meanDice
    -- * Casting 2D coordinates
  , RollDiceXY, rollDiceXY, castDiceXY, maxDiceXY, minDiceXY, meanDiceXY
    -- * Casting dependent on depth
  , RollDeep, rollDeep, castDeep, chanceDeep, intToDeep, maxDeep
    -- * Fractional chance
  , Chance, chance
    -- * Frequency distribution for casting dice
  , Dice
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Control.Monad.State as St
import Data.Binary
import qualified Data.Hashable as Hashable
import qualified Data.IntMap.Strict as IM
import Data.Ratio
import Data.Tuple
import GHC.Generics (Generic)
import qualified System.Random as R

import Game.LambdaHack.Common.Frequency

-- | The monad of computations with random generator state.
-- The lazy state monad is OK here: the state is small and regularly forced.
type Rnd a = St.State R.StdGen a

-- | Get a random object within a range with a uniform distribution.
randomR :: (R.Random a) => (a, a) -> Rnd a
randomR range = St.state $ R.randomR range

-- | Get a random object of a given type with a uniform distribution.
random :: (R.Random a) => Rnd a
random = St.state R.random

-- | Get any element of a list with equal probability.
oneOf :: [a] -> Rnd a
oneOf [] = assert `failure` "oneOf []" `twith` ()
oneOf xs = do
  r <- randomR (0, length xs - 1)
  return (xs !! r)

-- | Gen an element according to a frequency distribution.
frequency :: Show a => Frequency a -> Rnd a
frequency fr = St.state $ rollFreq fr

-- | Cast a single die.
cast :: Int -> Rnd Int
cast x = if x <= 0 then return 0 else randomR (1, x)

type Dice = Frequency Int

normalize :: Dice -> Dice
normalize dice = toFreq ("normalized" <+> nameFrequency dice)
                 $ map swap $ IM.toAscList $ IM.fromListWith (+)
                 $ map swap $ runFrequency dice

-- Normalized mainly as an optimization, but it also makes many expected
-- algeraic laws hold (wrt @Eq@), except for some laws about
-- multiplication.
instance Num Dice where
  fr1 + fr2 = normalize $ liftA2 (+) fr1 fr2  -- may be faster than @liftM2@
  fr1 * fr2 = do
    n <- fr1
    sum <$> replicateM n fr2  -- not commutative!
  fr1 - fr2 = normalize $ liftA2 (-) fr1 fr2
  negate = liftA negate
  abs = normalize . liftA abs
  signum = normalize . liftA signum
  fromInteger = pure . fromInteger

-- | Dice: 1d7, 3d3, 1d0, etc.
-- @RollDice a b@ represents @a@ rolls of @b@-sided die.
data RollDice = RollDice !Word8 !Word8
  deriving (Eq, Ord, Generic)

instance Show RollDice where
  show (RollDice a b) = show a ++ "d" ++ show b

instance Read RollDice where
  readsPrec d s =
    let (a, db) = break (== 'd') s
        av = read a
    in case db of
      'd' : b -> [ (RollDice av bv, rest) | (bv, rest) <- readsPrec d b ]
      _ -> []

instance Hashable.Hashable RollDice

instance Binary RollDice

rollDice :: Int -> Int -> RollDice
rollDice a b = assert (a >= 0 && a <= 255 && b >= 0 && b <= 255
                       `blame` "dice out of bounds" `twith` (a, b))
               $ RollDice (toEnum a) (toEnum b)

-- | Cast dice and sum the results.
castDice :: RollDice -> Rnd Int
castDice (RollDice a' 1)  = return $! fromEnum a'  -- optimization
castDice (RollDice a' b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in liftM sum (replicateM a (cast b))

-- | Maximal value of dice.
maxDice :: RollDice -> Int
maxDice (RollDice a' b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in a * b

-- | Minimal value of dice.
minDice :: RollDice -> Int
minDice (RollDice a' b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in if b == 0 then 0 else a

-- | Mean value of dice.
meanDice :: RollDice -> Rational
meanDice (RollDice a' b') =
  let (a, b) = (fromIntegral a', fromIntegral b')
  in if b' == 0 then 0 else a * (b + 1) % 2

-- | Dice for rolling a pair of integer parameters pertaining to,
-- respectively, the X and Y cartesian 2D coordinates.
data RollDiceXY = RollDiceXY ![RollDice] ![RollDice]
  deriving Show

rollDiceXY :: [(Int, Int)] -> [(Int, Int)] -> RollDiceXY
rollDiceXY lx ly = RollDiceXY (map (uncurry rollDice) lx)
                              (map (uncurry rollDice) ly)

-- | Cast the two sets of dice.
castDiceXY :: RollDiceXY -> Rnd (Int, Int)
castDiceXY (RollDiceXY lx ly) = do
  cx <- mapM castDice lx
  cy <- mapM castDice ly
  return (sum cx, sum cy)

-- | Maximal value of RollDiceXY.
maxDiceXY :: RollDiceXY -> (Int, Int)
maxDiceXY (RollDiceXY lx ly) = (sum (map maxDice lx), sum (map maxDice ly))

-- | Minimal value of RollDiceXY.
minDiceXY :: RollDiceXY -> (Int, Int)
minDiceXY (RollDiceXY lx ly) = (sum (map minDice lx), sum (map minDice ly))

-- | Mean value of RollDiceXY.
meanDiceXY :: RollDiceXY -> (Rational, Rational)
meanDiceXY (RollDiceXY lx ly) = (sum (map meanDice lx), sum (map meanDice ly))

-- | Dice for parameters scaled with current level depth.
-- To the result of rolling the first set of dice we add the second,
-- scaled in proportion to current depth divided by maximal dungeon depth.
data RollDeep = RollDeep !RollDice !RollDice
  deriving (Show, Eq, Ord, Generic)

instance Binary RollDeep

rollDeep :: (Int, Int) -> (Int, Int) -> RollDeep
rollDeep (a, b) (c, d) = RollDeep (rollDice a b) (rollDice c d)

-- | Cast dice scaled with current level depth.
-- Note that at the first level, the scaled dice are always ignored.
castDeep :: Int -> Int -> RollDeep -> Rnd Int
castDeep n' depth' (RollDeep d1 d2) = do
  let n = abs n'
      depth = abs depth'
  assert (n > 0 && n <= depth `blame` "invalid current depth for dice rolls"
                              `twith` (n, depth)) skip
  r1 <- castDice d1
  r2 <- castDice d2
  return $! r1 + ((n - 1) * r2) `div` max 1 (depth - 1)

-- | Cast dice scaled with current level depth and return @True@
-- if the results if greater than 50.
chanceDeep :: Int -> Int -> RollDeep -> Rnd Bool
chanceDeep n' depth' deep = do
  let n = abs n'
      depth = abs depth'
  c <- castDeep n depth deep
  return $! c > 50

-- | Generate a @RollDeep@ that always gives a constant integer.
intToDeep :: Int -> RollDeep
intToDeep 0  = RollDeep (RollDice 0 0) (RollDice 0 0)
intToDeep n' = if n' > fromEnum (maxBound :: Word8)
                  || n' < fromEnum (minBound :: Word8)
               then assert `failure` "Deep out of bound" `twith` n'
               else let n = toEnum n'
                    in RollDeep (RollDice n 1) (RollDice 0 0)

-- | Maximal value of scaled dice.
maxDeep :: RollDeep -> Int
maxDeep (RollDeep d1 d2) = maxDice d1 + maxDice d2

-- | Fractional chance.
type Chance = Rational

-- | Give @True@, with probability determined by the fraction.
chance :: Chance -> Rnd Bool
chance r = do
  let n = numerator r
      d = denominator r
  k <- randomR (1, d)
  return (k <= n)
