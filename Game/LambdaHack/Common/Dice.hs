{-# LANGUAGE DeriveGeneric, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Representation of dice for parameters scaled with current level depth.
module Game.LambdaHack.Common.Dice
  ( -- * Frequency distribution for casting dice scaled with level depth
    Dice, d, z, dl, zl
  , maxDice, minDice, meanDice
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import Control.Monad
import Data.Binary
import qualified Data.Hashable as Hashable
import qualified Data.IntMap.Strict as IM
import Data.Ratio
import Data.Tuple
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Msg

type SimpleDice = Frequency Int

normalizeSimple :: SimpleDice -> SimpleDice
normalizeSimple fr = toFreq ("normalized" <+> nameFrequency fr)
                     $ map swap $ IM.toAscList $ IM.fromListWith (+)
                     $ map swap $ runFrequency fr

-- Normalized mainly as an optimization, but it also makes many expected
-- algeraic laws hold (wrt @Eq@), except for some laws about
-- multiplication.
instance Num SimpleDice where
  fr1 + fr2 = normalizeSimple $ liftA2 (+) fr1 fr2  -- faster than @liftM2@
  fr1 * fr2 = do
    n <- fr1
    sum <$> replicateM n fr2  -- not commutative!
  fr1 - fr2 = normalizeSimple $ liftA2 (-) fr1 fr2
  negate = liftA negate
  abs = normalizeSimple . liftA abs
  signum = normalizeSimple . liftA signum
  fromInteger = pure . fromInteger

dieSimple :: Int -> SimpleDice
dieSimple n = uniformFreq "dieSimple" [1..n]

zdieSimple :: Int -> SimpleDice
zdieSimple n = uniformFreq "dieSimple" [1..n-1]

-- | Dice for parameters scaled with current level depth.
-- To the result of rolling the first set of dice we add the second,
-- scaled in proportion to current depth divided by maximal dungeon depth.
data Dice = Dice
  { diceConst :: Frequency Int
  , diceLevel :: Frequency Int
  }
  deriving (Show, Eq)

instance Num Dice where
  (Dice dc1 ds1) + (Dice dc2 ds2) = Dice (dc1 + dc2) (ds1 + ds2)
  (Dice dc1 ds1) * (Dice dc2 ds2) = Dice (dc1 * dc2) (ds1 * ds2)
  (Dice dc1 ds1) - (Dice dc2 ds2) = Dice (dc1 - dc2) (ds1 - ds2)
  negate = affectBothDice negate
  abs = affectBothDice abs
  signum = affectBothDice signum
  fromInteger n = Dice (fromInteger n) (fromInteger 0)

affectBothDice :: (Frequency Int -> Frequency Int) -> Dice -> Dice
affectBothDice f (Dice dc ds) = Dice (f dc) (f ds)

d :: Int -> Dice
d n = Dice (dieSimple n) (fromInteger 0)

z :: Int -> Dice
z n = Dice (zdieSimple n) (fromInteger 0)

dl :: Int -> Dice
dl n = Dice (fromInteger 0) (dieSimple n)

zl :: Int -> Dice
zl n = Dice (fromInteger 0) (zdieSimple n)

-- | Maximal value of dice. The scaled part taken assuming maximum level.
-- Assumes the frequencies are not null.
maxDice :: Dice -> Int
maxDice Dice{..} = maxFreq diceConst + maxFreq diceLevel

-- | Minimal value of dice. The scaled part ignored.
-- Assumes the frequencies are not null.
minDice :: Dice -> Int
minDice Dice{diceConst} = minFreq diceConst

-- | Mean value of dice. The scaled part taken assuming average level.
-- Assumes the frequencies are not null.
meanDice :: Dice -> Rational
meanDice Dice{..} = meanFreq diceConst + meanFreq diceLevel * (1%2)
