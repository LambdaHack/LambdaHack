{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Representation of dice scaled with current level depth.
module Game.LambdaHack.Core.Dice
  ( -- * Frequency distribution for casting dice scaled with level depth
    Dice, AbsDepth(..), castDice, d, dL, z, zL, intToDice, minDice, maxDice
  , infsupDice, supDice, infDice, meanDice, reduceDice
    -- * Dice for rolling a pair of integer parameters representing coordinates.
  , DiceXY(..), supDiceXY, infDiceXY
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Binary

-- | Multiple dice rolls, some scaled with current level depth, in which case
-- the sum of all rolls is scaled in proportion to current depth
-- divided by maximal dungeon depth.
--
-- The simple dice should have positive number of rolls and number of sides.
--
-- The @Num@ instance doesn't have @abs@ nor @signum@ defined,
-- because the functions for computing infimum, supremum and mean dice
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
  | DiceMin Dice Dice
  | DiceMax Dice Dice
  deriving Eq

instance Show Dice where
  show = stripOuterParens . showDiceWithParens

stripOuterParens :: String -> String
stripOuterParens s@('(' : rest) = case uncons $ reverse rest of
  Just (')', middle) -> reverse middle
  _ -> s
stripOuterParens s = s

showDiceWithParens :: Dice -> String
showDiceWithParens = sh
 where
  sh dice1 = case dice1 of
    DiceI k -> show k
    DiceD n k -> show n ++ "d" ++ show k
    DiceDL n k -> show n ++ "dL" ++ show k
    DiceZ n k -> show n ++ "z" ++ show k
    DiceZL n k -> show n ++ "zL" ++ show k
    DicePlus d1 (DiceNegate d2) -> wrapInParens $ sh d1 ++ "-" ++ sh d2
    DicePlus (DiceNegate d1) d2 -> wrapInParens $ "-" ++ sh d1 ++ "+" ++ sh d2
    DicePlus d1 (DicePlus d2 d3) -> sh $ DicePlus (DicePlus d1 d2) d3
    DicePlus (DicePlus d1 d2) d3 ->
      wrapInParens $ stripOuterParens (sh $ DicePlus d1 d2) ++ "+" ++ sh d3
    DicePlus d1 d2 -> wrapInParens $ sh d1 ++ "+" ++ sh d2
    DiceTimes d1 d2 -> wrapInParens $ sh d1 ++ "*" ++ sh d2
    DiceNegate d1 -> wrapInParens $ "-" ++ sh d1
    DiceMin d1 d2 -> wrapInParens $ "min" ++ sh d1 ++ sh d2
    DiceMax d1 d2 -> wrapInParens $ "max" ++ sh d1 ++ sh d2

wrapInParens :: String -> String
wrapInParens "" = ""
wrapInParens t = "(" <> t <> ")"

instance Num Dice where
  d1 + d2 = DicePlus d1 d2
  d1 * d2 = DiceTimes d1 d2
  d1 - d2 = d1 + DiceNegate d2
  negate = DiceNegate
  abs = undefined  -- very costly to compute mean exactly
  signum = undefined  -- very costly to compute mean exactly
  fromInteger n = DiceI (fromInteger n)

-- | Absolute depth in the dungeon. When used for the maximum depth
-- of the whole dungeon, this can be different than dungeon size,
-- e.g., when the dungeon is branched, and it can even be different
-- than the length of the longest branch, if levels at some depths are missing.
newtype AbsDepth = AbsDepth Int
  deriving (Show, Eq, Ord, Binary)

-- | Cast dice scaled with current level depth. When scaling, we round up,
-- so that the value of @1 `dL` 1@ is @1@ even at the lowest level,
-- but so is the value of @1 `dL` depth@.
--
-- The implementation calls RNG as many times as there are dice rolls,
-- which is costly, so content should prefer to cast fewer dice
-- and then multiply them by a constant. If rounded results are not desired
-- (often they are, to limit the number of distinct item varieties
-- in inventory), another dice may be added to the result.
--
-- A different possible implementation, with dice represented as @Frequency@,
-- makes only one RNG call per dice, but due to lists lengths proportional
-- to the maximal value of the dice, it's is intractable for 1000d1000
-- and problematic already for 100d100.
castDice :: forall m. Monad m
         => ((Int, Int) -> m Int)
         -> AbsDepth -> AbsDepth -> Dice -> m Int
{-# INLINE castDice #-}
castDice randomR (AbsDepth lvlDepth) (AbsDepth maxDepth) dice = do
  let !_A = assert (lvlDepth >= 0 && lvlDepth <= maxDepth
                    `blame` "invalid depth for dice rolls"
                    `swith` (lvlDepth, maxDepth)) ()
      castNK n start k = if start == k then return $! n * k else do
          let f !acc 0 = return acc
              f acc count = do
                r <- randomR (start, k)
                f (acc + r) (count - 1)
          f 0 n
      scaleL k = (k * max 1 lvlDepth) `divUp` max 1 maxDepth
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
        DiceMin d1 d2 -> do
          k1 <- castD d1
          k2 <- castD d2
          return $! min k1 k2
        DiceMax d1 d2 -> do
          k1 <- castD d1
          k2 <- castD d2
          return $! max k1 k2
  castD dice

-- | A die, rolled the given number of times. E.g., @1 `d` 2@ rolls 2-sided
-- die one time.
d :: Int -> Int -> Dice
d n k = assert (n > 0 && k > 0 `blame` "die must be positive" `swith` (n, k))
        $ DiceD n k

-- | A die rolled the given number of times,
-- with the result scaled with dungeon level depth.
dL :: Int -> Int -> Dice
dL n k = assert (n > 0 && k > 0 `blame` "die must be positive" `swith` (n, k))
         $ DiceDL n k

-- | A die, starting from zero, ending at one less than second argument,
-- rolled the given number of times. E.g., @1 `z` 1@ always rolls zero.
z :: Int -> Int -> Dice
z n k = assert (n > 0 && k > 0 `blame` "die must be positive" `swith` (n, k))
        $ DiceZ n k

-- | A die, starting from zero, ending at one less than second argument,
-- rolled the given number of times,
-- with the result scaled with dungeon level depth.
zL :: Int -> Int -> Dice
zL n k = assert (n > 0 && k > 0 `blame` "die must be positive" `swith` (n, k))
         $ DiceZL n k

intToDice :: Int -> Dice
intToDice = DiceI

minDice :: Dice -> Dice -> Dice
minDice = DiceMin

maxDice :: Dice -> Dice -> Dice
maxDice = DiceMax

-- | Minimal and maximal possible value of the dice.
--
-- @divUp@ in the implementation corresponds to @ceiling@,
-- applied to results of @meanDice@ elsewhere in the code,
-- and prevents treating 1d1-power effects (on shallow levels) as null effects.
infsupDice :: Dice -> (Int, Int)
infsupDice dice1 = case dice1 of
  DiceI k -> (k, k)
  DiceD n k -> (n, n * k)
  DiceDL n k -> (1, n * k)  -- bottom and top level considered
  DiceZ n k -> (0, n * (k - 1))
  DiceZL n k -> (0, n * (k - 1))  -- bottom and top level considered
  DicePlus d1 d2 ->
    let (infD1, supD1) = infsupDice d1
        (infD2, supD2) = infsupDice d2
    in (infD1 + infD2, supD1 + supD2)
  DiceTimes (DiceI k) d2 ->
    let (infD2, supD2) = infsupDice d2
    in if k >= 0 then (k * infD2, k * supD2) else (k * supD2, k * infD2)
  DiceTimes d1 (DiceI k) ->
    let (infD1, supD1) = infsupDice d1
    in if k >= 0 then (infD1 * k, supD1 * k) else (supD1 * k, infD1 * k)
  -- Multiplication other than the two cases above is unlikely, but here it is.
  DiceTimes d1 d2 ->
    let (infD1, supD1) = infsupDice d1
        (infD2, supD2) = infsupDice d2
        options = [infD1 * infD2, infD1 * supD2, supD1 * supD2, supD1 * infD2]
    in (minimum options, maximum options)
  DiceNegate d1 ->
    let (infD1, supD1) = infsupDice d1
    in (negate supD1, negate infD1)
  DiceMin d1 d2 ->
    let (infD1, supD1) = infsupDice d1
        (infD2, supD2) = infsupDice d2
    in (min infD1 infD2, min supD1 supD2)
  DiceMax d1 d2 ->
    let (infD1, supD1) = infsupDice d1
        (infD2, supD2) = infsupDice d2
    in (max infD1 infD2, max supD1 supD2)

-- | Maximal value of dice. The scaled part taken assuming median level.
supDice :: Dice -> Int
supDice = snd . infsupDice

-- | Minimal value of dice. The scaled part taken assuming median level.
infDice :: Dice -> Int
infDice = fst . infsupDice

-- | Mean value of dice. The scaled part taken assuming median level,
-- but not taking into account rounding up, and so too low, especially
-- for dice small compared to depth. To fix this, depth would need
-- to be taken as argument.
meanDice :: Dice -> Double
meanDice dice1 = case dice1 of
  DiceI k -> intToDouble k
  DiceD n k -> intToDouble (n * (k + 1)) / 2
  DiceDL n k -> intToDouble (n * (k + 1)) / 4
  DiceZ n k -> intToDouble (n * k) / 2
  DiceZL n k -> intToDouble (n * k) / 4
  DicePlus d1 d2 -> meanDice d1 + meanDice d2
  DiceTimes d1 d2 -> meanDice d1 * meanDice d2  -- I hope this is that simple
  DiceNegate d1 -> negate $ meanDice d1
  DiceMin d1 d2 -> min (meanDice d1) (meanDice d2)
    -- crude approximation, only exact if the distributions disjoint
  DiceMax d1 d2 -> max (meanDice d1) (meanDice d2)  -- crude approximation

reduceDice :: Dice -> Maybe Int
reduceDice d1 =
  let (infD1, supD1) = infsupDice d1
  in if infD1 == supD1 then Just infD1 else Nothing

-- | Dice for rolling a pair of integer parameters pertaining to,
-- respectively, the X and Y cartesian 2D coordinates.
data DiceXY = DiceXY Dice Dice
  deriving Show

-- | Maximal value of DiceXY.
supDiceXY :: DiceXY -> (Int, Int)
supDiceXY (DiceXY x y) = (supDice x, supDice y)

-- | Minimal value of DiceXY.
infDiceXY :: DiceXY -> (Int, Int)
infDiceXY (DiceXY x y) = (infDice x, infDice y)
