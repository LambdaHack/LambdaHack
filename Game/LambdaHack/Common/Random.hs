{-# LANGUAGE DeriveGeneric #-}
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
  ) where

import Control.Exception.Assert.Sugar
import qualified Control.Monad.State as St
import Data.Ratio
import qualified System.Random as R

import qualified Game.LambdaHack.Common.Dice as Dice
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

-- | Randomly choose an item according to the distribution.
rollFreq :: Show a => Frequency a -> R.StdGen -> (a, R.StdGen)
rollFreq fr g = case runFrequency fr of
  [] -> assert `failure` "choice from an empty frequency"
               `twith` nameFrequency fr
  [(n, x)] | n <= 0 -> assert `failure` "singleton void frequency"
                                 `twith` (nameFrequency fr, n, x)
  [(_, x)] -> (x, g)  -- speedup
  fs -> let sumf = sum (map fst fs)
            (r, ng) = R.randomR (1, sumf) g
            frec :: Int -> [(Int, a)] -> a
            frec m [] = assert `failure` "impossible roll"
                               `twith` (nameFrequency fr, fs, m)
            frec m ((n, x) : _)  | m <= n = x
            frec m ((n, _) : xs) = frec (m - n) xs
        in assert (sumf > 0 `blame` "frequency with nothing to pick"
                            `twith` (nameFrequency fr, fs))
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
castDice :: Int -> Int -> Dice.Dice -> Rnd Int
castDice n' maxDepth' dice = do
  let n = abs n'
      maxDepth = abs maxDepth'
  assert (n >= 0 && n <= maxDepth `blame` "invalid depth for dice rolls"
                                  `twith` (n', maxDepth')) skip
  dc <- frequency $ Dice.diceConst dice
  dl <- frequency $ Dice.diceLevel dice
  return $! dc + (dl * max 0 (n - 1)) `div` max 1 (maxDepth - 1)

-- | Cast dice scaled with current level depth and return @True@
-- if the results if greater than 50.
chanceDice :: Int -> Int -> Dice.Dice -> Rnd Bool
chanceDice n' maxDepth' dice = do
  c <- castDice n' maxDepth' dice
  return $! c > 50

-- | Cast dice, scaled with current level depth, for coordinates.
castDiceXY :: Int -> Int -> Dice.DiceXY -> Rnd (Int, Int)
castDiceXY n' maxDepth' (Dice.DiceXY dx dy) = do
  x <- castDice n' maxDepth' dx
  y <- castDice n' maxDepth' dy
  return (x, y)
