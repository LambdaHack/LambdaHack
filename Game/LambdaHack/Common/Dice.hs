{-# LANGUAGE DeriveGeneric, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Representation of dice for parameters scaled with current level depth.
module Game.LambdaHack.Common.Dice
  ( -- * Frequency distribution for casting dice scaled with level depth
    Dice, diceConst, diceLevel, diceMult, (|*|)
  , d, dl, intToDice
  , maxDice, minDice, meanDice, reduceDice
    -- * Dice for rolling a pair of integer parameters representing coordinates.
  , DiceXY(..), maxDiceXY, minDiceXY, meanDiceXY
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , SimpleDice
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Applicative
import Control.DeepSeq
import Data.Binary
import qualified Data.Char as Char
import Data.Hashable (Hashable)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import Data.Tuple
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Frequency

type SimpleDice = Frequency Int

normalizeSimple :: SimpleDice -> SimpleDice
normalizeSimple fr = toFreq (nameFrequency fr)
                     $ map swap $ IM.toAscList $ IM.fromListWith (+)
                     $ map swap $ runFrequency fr

-- Normalized mainly as an optimization, but it also makes many expected
-- algebraic laws hold (wrt @Eq@), except for some laws about
-- multiplication. We use @liftA2@ instead of @liftM2@, because it's probably
-- faster in this case.
instance Num SimpleDice where
  fr1 + fr2 = normalizeSimple $ liftA2AdditiveName "+" (+) fr1 fr2
  fr1 * fr2 = undefined
  fr1 - fr2 = normalizeSimple $ liftA2AdditiveName "-" (-) fr1 fr2
  negate = liftAName "-" negate
  abs = normalizeSimple . liftAName "abs" abs
  signum = normalizeSimple . liftAName "signum" signum
  fromInteger n = renameFreq (tshow n) $ pure $ fromInteger n

mult :: SimpleDice -> SimpleDice -> SimpleDice
mult fr1 fr2 =
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

liftAName :: Text -> (Int -> Int) -> SimpleDice -> SimpleDice
liftAName name f fr =
  let frRes = f <$> fr
      nameRes = name <> " (" <> nameFrequency fr  <> ")"
  in renameFreq nameRes frRes

liftA2AdditiveName :: Text
                   -> (Int -> Int -> Int)
                   -> SimpleDice -> SimpleDice -> SimpleDice
liftA2AdditiveName name f fra frb =
  let frRes = liftA2 f fra frb
      nameRes
        | nameFrequency fra == "0" =
          (if name == "+" then "" else name) <+> nameFrequency frb
        | nameFrequency frb == "0" = nameFrequency fra
        | otherwise = nameFrequency fra <+> name <+> nameFrequency frb
  in renameFreq nameRes frRes

dieSimple :: Int -> SimpleDice
dieSimple n = uniformFreq ("d" <> tshow n) [1..n]

zdieSimple :: Int -> SimpleDice
zdieSimple n = uniformFreq ("z" <> tshow n) [0..n-1]

dieLevelSimple :: Int -> SimpleDice
dieLevelSimple n = uniformFreq ("dl" <> tshow n) [1..n]

zdieLevelSimple :: Int -> SimpleDice
zdieLevelSimple n = uniformFreq ("zl" <> tshow n) [0..n-1]

-- | Dice for parameters scaled with current level depth.
-- To the result of rolling the first set of dice we add the second,
-- scaled in proportion to current depth divided by maximal dungeon depth.
-- The result if then multiplied by the scale --- to be used to ensure
-- that dice results are multiples of, e.g., 10. The scale is set with @|*|@.
data Dice = Dice
  { diceConst :: SimpleDice
  , diceLevel :: SimpleDice
  , diceMult  :: Int
  }
  deriving (Eq, Ord, Generic)

instance Show Dice where
  show Dice{..} = T.unpack $
    let rawMult = nameFrequency diceLevel
        scaled = if rawMult == "0" then "" else rawMult
        signAndMult = case T.uncons scaled of
          Just ('-', _) -> scaled
          _ -> "+" <+> scaled
    in (if | nameFrequency diceLevel == "0" -> nameFrequency diceConst
           | nameFrequency diceConst == "0" -> scaled
           | otherwise -> nameFrequency diceConst <+> signAndMult)
       <+> if diceMult == 1 then "" else "|*|" <+> tshow diceMult

instance Hashable Dice

instance Binary Dice

instance NFData Dice

instance Num Dice where
  (Dice dc1 dl1 ds1) + (Dice dc2 dl2 ds2) =
    Dice (scaleFreq ds1 dc1 + scaleFreq ds2 dc2)
         (scaleFreq ds1 dl1 + scaleFreq ds2 dl2)
         (if ds1 == 1 && ds2 == 1 then 1 else
            error $ "|*| must be at top level" `showFailure` (ds1, ds2))
  (Dice dc1 dl1 ds1) * (Dice dc2 dl2 ds2) = undefined
  (Dice dc1 dl1 ds1) - (Dice dc2 dl2 ds2) =
    Dice (scaleFreq ds1 dc1 - scaleFreq ds2 dc2)
         (scaleFreq ds1 dl1 - scaleFreq ds2 dl2)
         (if ds1 == 1 && ds2 == 1 then 1 else
            error $ "|*| must be at top level" `showFailure` (ds1, ds2))
  negate = affectBothDice negate
  abs = affectBothDice abs
  signum = affectBothDice signum
  fromInteger n = Dice (fromInteger n) 0 1

affectBothDice :: (SimpleDice -> SimpleDice) -> Dice -> Dice
affectBothDice f (Dice dc1 dl1 ds1) = Dice (f dc1) (f dl1) ds1

-- | A single simple dice.
d :: Int -> Int -> Dice
d n k = Dice (mult (fromInteger $ toInteger n) (dieSimple k)) 0 1

-- | Dice scaled with level.
dl :: Int -> Int -> Dice
dl n k = Dice 0 (mult (fromInteger $ toInteger n) (dieLevelSimple k)) 1

-- Not exposed to save on documentation.
_z :: Int -> Int -> Dice
_z n k = Dice (mult (fromInteger $ toInteger n) (zdieSimple k)) 0 1

_zl :: Int -> Int -> Dice
_zl n k = Dice 0 (mult (fromInteger $ toInteger n) (zdieLevelSimple k)) 1

intToDice :: Int -> Dice
intToDice = fromInteger . fromIntegral

infixl 5 |*|
-- | Multiplying the dice, after all randomness is resolved, by a constant.
-- Infix declaration ensures that @1 + 2 |*| 3@ parses as @(1 + 2) |*| 3@.
(|*|) :: Dice -> Int -> Dice
Dice dc1 dl1 ds1 |*| s2 = Dice dc1 dl1 (ds1 * s2)

-- | Maximal value of dice. The scaled part taken assuming median level.
maxDice :: Dice -> Int
maxDice Dice{..} = (fromMaybe 0 (maxFreq diceConst)
                    + fromMaybe 0 (maxFreq diceLevel) `div` 2)
                   * diceMult

-- | Minimal value of dice. The scaled part taken assuming median level.
minDice :: Dice -> Int
minDice Dice{..} = (fromMaybe 0 (minFreq diceConst)
                    + fromMaybe 0 (minFreq diceLevel) `div` 2)
                   * diceMult

-- | Mean value of dice. The scaled part taken assuming median level.
-- Assumes the frequencies are not null.
meanDice :: Dice -> Int
meanDice Dice{..} = (meanFreq diceConst
                     + meanFreq diceLevel `div` 2)
                    * diceMult

reduceDice :: Dice -> Maybe Int
reduceDice de =
  let minD = minDice de
  in if minD == maxDice de then Just minD else Nothing

-- | Dice for rolling a pair of integer parameters pertaining to,
-- respectively, the X and Y cartesian 2D coordinates.
data DiceXY = DiceXY Dice Dice
  deriving (Show, Eq, Ord, Generic)

instance Hashable DiceXY

instance Binary DiceXY

-- | Maximal value of DiceXY.
maxDiceXY :: DiceXY -> (Int, Int)
maxDiceXY (DiceXY x y) = (maxDice x, maxDice y)

-- | Minimal value of DiceXY.
minDiceXY :: DiceXY -> (Int, Int)
minDiceXY (DiceXY x y) = (minDice x, minDice y)

-- | Mean value of DiceXY.
meanDiceXY :: DiceXY -> (Int, Int)
meanDiceXY (DiceXY x y) = (meanDice x, meanDice y)
