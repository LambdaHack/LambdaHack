{-# LANGUAGE DeriveGeneric, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Representation of dice for parameters scaled with current level depth.
module Game.LambdaHack.Common.Dice
  ( -- * Frequency distribution for casting dice scaled with level depth
    Dice, diceConst, diceLevel
  , d, z, dl, zl, intToDice
  , maxDice, minDice, meanDice
    -- * Dice for rolling a pair of integer parameters representing coordinates.
  , DiceXY(..), maxDiceXY, minDiceXY, meanDiceXY
  ) where

import Control.Applicative
import Data.Binary
import qualified Data.Char as Char
import qualified Data.Hashable as Hashable
import qualified Data.IntMap.Strict as IM
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Msg

type SimpleDice = Frequency Int

normalizeSimple :: SimpleDice -> SimpleDice
normalizeSimple fr = toFreq (nameFrequency fr)
                     $ map swap $ IM.toAscList $ IM.fromListWith (+)
                     $ map swap $ runFrequency fr

-- Normalized mainly as an optimization, but it also makes many expected
-- algeraic laws hold (wrt @Eq@), except for some laws about
-- multiplication. We use @liftA2@ instead of @liftM2@, because it's probably
-- faster in this case.
instance Num SimpleDice where
  fr1 + fr2 = normalizeSimple $ liftA2AdditiveName "+" (+) fr1 fr2
  fr1 * fr2 =
    let frRes = normalizeSimple $ do
          n <- fr1
          sum $ replicate n fr2  -- not commutative!
        nameRes =
          case T.uncons $ nameFrequency fr2 of
            _ | nameFrequency fr1 == "0" || nameFrequency fr2 == "0" -> "0"
            Just ('d', _) | T.all Char.isDigit $ nameFrequency fr1 ->
              nameFrequency fr1 <> nameFrequency fr2
            _ -> nameFrequency fr1 <+> "*" <+> nameFrequency fr2
    in renameFreq nameRes frRes
  fr1 - fr2 = normalizeSimple $ liftA2AdditiveName "-" (-) fr1 fr2
  negate = liftAName "-" negate
  abs = normalizeSimple . liftAName "abs" abs
  signum = normalizeSimple . liftAName "signum" signum
  fromInteger n = renameFreq (tshow n) $ pure $ fromInteger n

instance Show SimpleDice where
  show = T.unpack . nameFrequency

liftAName :: Text -> (Int -> Int) -> SimpleDice -> SimpleDice
liftAName name f fr =
  let frRes = liftA f fr
      nameRes = name <> " (" <> nameFrequency fr  <> ")"
  in renameFreq nameRes frRes

liftA2AdditiveName :: Text
                   -> (Int -> Int -> Int)
                   -> SimpleDice -> SimpleDice -> SimpleDice
liftA2AdditiveName name f fra frb =
  let frRes = liftA2 f fra frb
      nameRes =
        if nameFrequency fra == "0" then nameFrequency frb
        else if nameFrequency frb == "0" then nameFrequency fra
        else nameFrequency fra <+> name <+> nameFrequency frb
  in renameFreq nameRes frRes

dieSimple :: Int -> SimpleDice
dieSimple n = uniformFreq ("d" <> tshow n) [1..n]

zdieSimple :: Int -> SimpleDice
zdieSimple n = uniformFreq ("z" <> tshow n) [0..n-1]

-- | Dice for parameters scaled with current level depth.
-- To the result of rolling the first set of dice we add the second,
-- scaled in proportion to current depth divided by maximal dungeon depth.
data Dice = Dice
  { diceConst :: SimpleDice
  , diceLevel :: SimpleDice
  }
  deriving (Read, Eq, Ord, Generic)

instance Show Dice where
  show Dice{..} = T.unpack $
    let scaled = "scaled(" <> nameFrequency diceLevel <> ")"
    in if nameFrequency diceLevel == "0" then nameFrequency diceConst
       else if nameFrequency diceConst == "0" then scaled
       else nameFrequency diceConst <+> "+" <+> scaled

instance Hashable.Hashable Dice

instance Binary Dice

instance Num Dice where
  (Dice dc1 ds1) + (Dice dc2 ds2) = Dice (dc1 + dc2) (ds1 + ds2)
  (Dice dc1 ds1) * (Dice dc2 ds2) =
    -- Hacky, but necessary (unless we forgo general multiplication and
    -- stick to multiplications by a scalar from the left and from the right).
    -- The pseudo-reasoning goes (remember the multiplication
    -- is not commutative, so we take all kinds of liberties):
    -- (dc1 + ds1 * l) * (dc2 + ds2 * l)
    -- = dc1 * dc2 + dc1 * ds2 * l + ds1 * l * dc2 + ds1 * l * ds2 * l
    -- = dc1 * dc2 + (dc1 * ds2) * l + (ds1 * dc2) * l + (ds1 * ds2) * l * l
    -- Now, we don't have a slot to put the coefficient of l * l into
    -- (and we don't know l yet, so we can't eliminate it by division),
    -- so we happily ingore it. Done. It works well in the cases that interest
    -- us, that is, multiplication by a scalar (a one-element frequency
    -- distribution) from any side, unscaled and scaled by level depth
    -- (but when we multiply two scaled scalars, we get 0).
    Dice (dc1 * dc2) (dc1 * ds2 + ds1 * dc2)
  (Dice dc1 ds1) - (Dice dc2 ds2) = Dice (dc1 - dc2) (ds1 - ds2)
  negate = affectBothDice negate
  abs = affectBothDice abs
  signum = affectBothDice signum
  fromInteger n = Dice (fromInteger n) (fromInteger 0)

affectBothDice :: (SimpleDice -> SimpleDice) -> Dice -> Dice
affectBothDice f (Dice dc ds) = Dice (f dc) (f ds)

d :: Int -> Dice
d n = Dice (dieSimple n) (fromInteger 0)

z :: Int -> Dice
z n = Dice (zdieSimple n) (fromInteger 0)

dl :: Int -> Dice
dl n = Dice (fromInteger 0) (dieSimple n)

zl :: Int -> Dice
zl n = Dice (fromInteger 0) (zdieSimple n)

intToDice :: Int -> Dice
intToDice = fromInteger . fromIntegral

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

-- | Dice for rolling a pair of integer parameters pertaining to,
-- respectively, the X and Y cartesian 2D coordinates.
data DiceXY = DiceXY !Dice !Dice
  deriving (Show, Eq, Ord, Generic)

instance Hashable.Hashable DiceXY

instance Binary DiceXY

-- | Maximal value of DiceXY.
maxDiceXY :: DiceXY -> (Int, Int)
maxDiceXY (DiceXY x y) = (maxDice x, maxDice y)

-- | Minimal value of DiceXY.
minDiceXY :: DiceXY -> (Int, Int)
minDiceXY (DiceXY x y) = (minDice x, minDice y)

-- | Mean value of DiceXY.
meanDiceXY :: DiceXY -> (Rational, Rational)
meanDiceXY (DiceXY x y) = (meanDice x, meanDice y)
