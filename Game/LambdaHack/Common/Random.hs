-- | Representation of probabilities and random computations.
module Game.LambdaHack.Common.Random
  ( -- * The @Rng@ monad
    Rnd
    -- * Random operations
  , randomR, random, oneOf, frequency
    -- * Fractional chance
  , Chance, chance
    -- * Casting dice scaled with level
  , castDice, chanceDice, castDiceXY
    -- * Specialized monadic folds
  , foldrM, foldlM'
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , rollFreq
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import           Data.Ratio
import qualified System.Random as R

import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Frequency
import           Game.LambdaHack.Common.Misc

-- | The monad of computations with random generator state.
type Rnd a = St.State R.StdGen a

-- | Get a random object within a range with a uniform distribution.
randomR :: (R.Random a) => (a, a) -> Rnd a
{-# INLINE randomR #-}
randomR = St.state . R.randomR

-- | Get a random object of a given type with a uniform distribution.
random :: (R.Random a) => Rnd a
{-# INLINE random #-}
random = St.state R.random

-- | Get any element of a list with equal probability.
oneOf :: [a] -> Rnd a
oneOf [] = error $ "oneOf []" `showFailure` ()
oneOf xs = do
  r <- randomR (0, length xs - 1)
  return $! xs !! r

-- | Gen an element according to a frequency distribution.
frequency :: Show a => Frequency a -> Rnd a
{-# INLINE frequency #-}
frequency = St.state . rollFreq

-- | Randomly choose an item according to the distribution.
rollFreq :: Show a => Frequency a -> R.StdGen -> (a, R.StdGen)
rollFreq fr g = case runFrequency fr of
  [] -> error $ "choice from an empty frequency"
                `showFailure` nameFrequency fr
  [(n, x)] | n <= 0 -> error $ "singleton void frequency"
                               `showFailure` (nameFrequency fr, n, x)
  [(_, x)] -> (x, g)  -- speedup
  fs -> let sumf = foldl' (\ !acc (!n, _) -> acc + n) 0 fs
            (r, ng) = R.randomR (1, sumf) g
            frec :: Int -> [(Int, a)] -> a
            frec !m [] = error $ "impossible roll"
                                 `showFailure` (nameFrequency fr, fs, m)
            frec m ((n, x) : _) | m <= n = x
            frec m ((n, _) : xs) = frec (m - n) xs
        in assert (sumf > 0 `blame` "frequency with nothing to pick"
                            `swith` (nameFrequency fr, fs))
             (frec r fs, ng)

-- | Fractional chance.
type Chance = Rational

-- | Give @True@, with probability determined by the fraction.
chance :: Chance -> Rnd Bool
chance r = do
  let n = numerator r
      d = denominator r
  k <- randomR (1, d)
  return (k <= n)

-- | Cast dice scaled with current level depth.
-- Note that at the first level, the scaled dice are always ignored.
castDice :: AbsDepth -> AbsDepth -> Dice.Dice -> Rnd Int
castDice = Dice.castDice randomR

-- | Cast dice scaled with current level depth and return @True@
-- if the results is greater than 50.
chanceDice :: AbsDepth -> AbsDepth -> Dice.Dice -> Rnd Bool
chanceDice ldepth totalDepth dice = do
  c <- castDice ldepth totalDepth dice
  return $! c > 50

-- | Cast dice, scaled with current level depth, for coordinates.
castDiceXY :: AbsDepth -> AbsDepth -> Dice.DiceXY -> Rnd (Int, Int)
castDiceXY ldepth totalDepth (Dice.DiceXY dx dy) = do
  x <- castDice ldepth totalDepth dx
  y <- castDice ldepth totalDepth dy
  return (x, y)

foldrM :: Foldable t => (a -> b -> Rnd b) -> b -> t a -> Rnd b
foldrM f z0 xs = let f' x (z, g) = St.runState (f x z) g
                 in St.state $ \g -> foldr f' (z0, g) xs

foldlM' :: Foldable t => (b -> a -> Rnd b) -> b -> t a -> Rnd b
foldlM' f z0 xs = let f' (z, g) x = St.runState (f z x) g
                  in St.state $ \g -> foldl' f' (z0, g) xs
