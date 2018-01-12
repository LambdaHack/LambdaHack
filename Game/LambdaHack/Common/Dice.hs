{-# LANGUAGE DeriveGeneric, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Representation of dice scaled with current level depth.
module Game.LambdaHack.Common.Dice
  ( -- * Frequency distribution for casting dice scaled with level depth
    Dice, castDice, d, dL, z, zL, intToDice
  , minmaxDice, maxDice, minDice, meanDice, reduceDice
    -- * Dice for rolling a pair of integer parameters representing coordinates.
  , DiceXY(..), maxDiceXY, minDiceXY, meanDiceXY
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.DeepSeq
import Data.Binary
import Data.Hashable (Hashable)
import Game.LambdaHack.Common.Misc
import GHC.Generics (Generic)

-- | Multiple dice rolls, some scaled with current level depth, in which case
-- the sum of all rolls is scaled in proportion to current depth
-- divided by maximal dungeon depth.
--
-- The simple dice should have positive number of rolls and number of sides.
--
-- The @Num@ instance doesn't have @abs@ nor @signum@ defined,
-- because the functions for computing minimum, maximum and mean dice
-- results would be too costly.
data Dice =
    DiceI Int
  | DiceD Int Int
  | DiceDL Int Int
  | DiceZ Int Int
  | DiceZL Int Int
  | DicePlus Dice Dice
  | DiceTimes Dice Dice
  | DiceNegate Dice
  deriving (Eq, Ord, Generic)

instance Show Dice where
  show dice1 = case dice1 of
    DiceI k -> show k
    DiceD n k -> show n ++ "d" ++ show k
    DiceDL n k -> show n ++ "dL" ++ show k
    DiceZ n k -> show n ++ "z" ++ show k
    DiceZL n k -> show n ++ "zL" ++ show k
    DicePlus d1 (DiceNegate d2) | simpleDice d2 -> show d1 ++ "-" ++ show d2
    DicePlus d1 (DiceNegate d2) -> show d1 ++ "-" ++ "(" ++ show d2 ++ ")"
    DicePlus d1 d2 -> show d1 ++ "+" ++ show d2
    DiceTimes d1 d2 -> "(" ++ show d1 ++ ") * (" ++ show d2 ++ ")"  -- rare
    DiceNegate (DiceI k) -> "-" ++ show k  -- "-2" parses as this, not as DiceI
    DiceNegate d1 -> "- (" ++ show d1 ++ ")"

simpleDice :: Dice -> Bool
simpleDice DiceI{} = True
simpleDice DiceD{} = True
simpleDice DiceDL{} = True
simpleDice DiceZ{} = True
simpleDice DiceZL{} = True
simpleDice _ = False

instance Hashable Dice

instance Binary Dice

instance NFData Dice

instance Num Dice where
  d1 + d2 = DicePlus d1 d2
  d1 * d2 = DiceTimes d1 d2
  d1 - d2 = d1 + DiceNegate d2
  negate = DiceNegate
  abs = undefined
  signum = undefined
  fromInteger n = DiceI (fromInteger n)

-- | Cast dice scaled with current level depth.
-- Note that at the first level, the scaled dice are always ignored.
--
-- The implementation calls RNG as many times as there are dice rolls,
-- which is costly, so content should prefer to case fewer dice
-- and then multiply them by a constant. If rounded results are not desired
-- (often they are, to limit the number of distinct item varieties
-- in inventory), another dice may be added to the result.
--
-- A different possible implementation, with dice represented as 'Frequency',
-- makes only one RNG call per dice, but due to lists lengths proportional
-- to the maximal value of the dice, it's is intractable for 1000d1000
-- and problematic already for 100d100.
castDice :: forall m. Monad m
         => ((Int, Int) -> m Int)
         -> AbsDepth -> AbsDepth -> Dice -> m Int
castDice randomR (AbsDepth lvlDepth) (AbsDepth maxDepth) dice = do
  let !_A = assert (lvlDepth >= 0 && lvlDepth <= maxDepth
                    `blame` "invalid depth for dice rolls"
                    `swith` (lvlDepth, maxDepth)) ()
      castNK n start k = do
          let f !acc 0 = return acc
              f acc count = do
                r <- randomR (start, k)
                f (acc + r) (count - 1)
          f 0 n
      scaleL k = (k * max 0 (lvlDepth - 1)) `div` max 1 (maxDepth - 1)
      castD :: Dice -> m Int
      castD dice1 = case dice1 of
        DiceI k -> return k
        DiceD n k -> castNK n 1 k
        DiceDL n k -> scaleL <$> castNK n 1 k
        DiceZ n k -> castNK n 0 (k - 1)
        DiceZL n k -> scaleL <$> castNK n 0 (k - 1)
        DicePlus d1 d2 -> do
          k1 <- castD d1
          k2 <- castD d2
          return $! k1 + k2
        DiceTimes d1 d2 -> do
          k1 <- castD d1
          k2 <- castD d2
          return $! k1 * k2
        DiceNegate d1 -> do
          k <- castD d1
          return $! negate k
  castD dice

-- | A die, rolled the given number of times. E.g., @1 `d` 2@ rolls 2-sided
-- die one time.
d :: Int -> Int -> Dice
d n k = assert (n > 0 && k > 0 `blame` "die must be positive" `swith` (n, k))
        $ DiceD n k

-- | A die rolled the given number of times, with the result scaled
-- with dungeon level depth.
dL :: Int -> Int -> Dice
dL n k = assert (n > 0 && k > 0 `blame` "die must be positive" `swith` (n, k))
         $ DiceDL n k

-- | A die, starting from zero, ending at one less than the bound,
-- rolled the given number of times. E.g., @1 `z` 1@ always rolls zero.
z :: Int -> Int -> Dice
z n k = assert (n > 0 && k > 0 `blame` "die must be positive" `swith` (n, k))
        $ DiceZ n k

-- | A die, starting from zero, ending at one less than the bound,
-- rolled the given number of times,
-- with the result scaled with dungeon level depth.
zL :: Int -> Int -> Dice
zL n k = assert (n > 0 && k > 0 `blame` "die must be positive" `swith` (n, k))
         $ DiceZL n k

intToDice :: Int -> Dice
intToDice = DiceI

-- | Minimal and maximal possible value of the dice.
--
-- @divUp@ in the implementation corresponds to @ceiling@,
-- applied to results of @meanDice@ elsewhere in the code,
-- and prevents treating 1d1-power effects (on shallow levels) as null effects.
minmaxDice :: Dice -> (Int, Int)
minmaxDice dice1 = case dice1 of
  DiceI k -> (k, k)
  DiceD n k -> (n, n * k)
  DiceDL n k -> (0, n * k)  -- bottom and top level considered
  DiceZ n k -> (0, n * (k - 1))
  DiceZL n k -> (0, n * (k - 1))  -- bottom and top level considered
  DicePlus d1 d2 ->
    let (minD1, maxD1) = minmaxDice d1
        (minD2, maxD2) = minmaxDice d2
    in (minD1 + minD2, maxD1 + maxD2)
  DiceTimes (DiceI k) d2 ->
    let (minD2, maxD2) = minmaxDice d2
    in if k >= 0 then (k * minD2, k * maxD2) else (k * maxD2, k * minD2)
  DiceTimes d1 (DiceI k) ->
    let (minD1, maxD1) = minmaxDice d1
    in if k >= 0 then (minD1 * k, maxD1 * k) else (maxD1 * k, minD1 * k)
  -- Multiplication other than the two cases above is unlikely, but here it is.
  DiceTimes d1 d2 ->
    let (minD1, maxD1) = minmaxDice d1
        (minD2, maxD2) = minmaxDice d2
        options = [minD1 * minD2, minD1 * maxD2, maxD1 * maxD2, maxD1 * minD2]
    in (minimum options, maximum options)
  DiceNegate d1 ->
    let (minD1, maxD1) = minmaxDice d1
    in (negate maxD1, negate minD1)

-- | Maximal value of dice. The scaled part taken assuming median level.
maxDice :: Dice -> Int
maxDice = snd . minmaxDice

-- | Minimal value of dice. The scaled part taken assuming median level.
minDice :: Dice -> Int
minDice = fst . minmaxDice

-- | Mean value of dice. The scaled part taken assuming median level.
meanDice :: Dice -> Double
meanDice dice1 = case dice1 of
  DiceI k -> fromIntegral k
  DiceD n k -> fromIntegral (n * (k + 1)) / 2
  DiceDL n k -> fromIntegral (n * (k + 1)) / 4
  DiceZ n k -> fromIntegral (n * k) / 2
  DiceZL n k -> fromIntegral (n * k) / 4
  DicePlus d1 d2 -> meanDice d1 + meanDice d2
  DiceTimes d1 d2 -> meanDice d1 * meanDice d2  -- I hope this is that simple
  DiceNegate d1 -> negate $ meanDice d1

reduceDice :: Dice -> Maybe Int
reduceDice d1 =
  let (minD1, maxD1) = minmaxDice d1
  in if minD1 == maxD1 then Just minD1 else Nothing

-- | Dice for rolling a pair of integer parameters pertaining to,
-- respectively, the X and Y cartesian 2D coordinates.
data DiceXY = DiceXY Dice Dice
  deriving (Show, Generic)

instance Hashable DiceXY

instance Binary DiceXY

instance NFData DiceXY

-- | Maximal value of DiceXY.
maxDiceXY :: DiceXY -> (Int, Int)
maxDiceXY (DiceXY x y) = (maxDice x, maxDice y)

-- | Minimal value of DiceXY.
minDiceXY :: DiceXY -> (Int, Int)
minDiceXY (DiceXY x y) = (minDice x, minDice y)

-- | Mean value of DiceXY.
meanDiceXY :: DiceXY -> (Double, Double)
meanDiceXY (DiceXY x y) = (meanDice x, meanDice y)
