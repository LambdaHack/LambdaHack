-- | Representation of probabilities and random computations.
module Game.LambdaHack.Core.Random
  ( -- * The @Rng@ monad
    Rnd
    -- * Random operations
  , randomR, randomR0, nextRandom, randomWord32
  , oneOf, shuffle, shuffleExcept, frequency
    -- * Fractional chance
  , Chance, chance
    -- * Casting dice scaled with level
  , castDice, oddsDice, castDiceXY
    -- * Specialized monadic folds
  , foldrM, foldlM'
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , rollFreq
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import           Data.Int (Int32)
import           Data.Ratio
import qualified Data.Vector.Unboxed as U
import           Data.Word (Word16, Word32)
import qualified System.Random.SplitMix32 as SM

import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Frequency

-- | The monad of computations with random generator state.
type Rnd a = St.State SM.SMGen a

-- | Get a random object within a (inclusive) range with a uniform distribution.
randomR :: (Integral a) => (a, a) -> Rnd a
{-# INLINE randomR #-}
randomR (0, h) = St.state $ nextRandom h
randomR (l, h) | l > h = error "randomR: empty range"
randomR (l, h) = St.state $ \g ->
  let (x, g') = nextRandom (h - l) g
  in (x + l, g')

-- | Generate random 'Integral' in @[0, x]@ range.
randomR0 :: (Integral a) => a -> Rnd a
{-# INLINE randomR0 #-}
randomR0 h = St.state $ nextRandom h

-- | Generate a random integral value in @[0, x]@ range, where @x@ is within
-- @Int32@.
--
-- The limitation to @Int32@ values is needed to keep it working on signed
-- types. In package @random@, a much more complex scheme is used
-- to keep it working for arbitrary fixed number of bits.
nextRandom :: forall a. (Integral a) => a -> SM.SMGen -> (a, SM.SMGen)
{-# INLINE nextRandom #-}
nextRandom h g = assert (toInteger h
                         <= (toInteger :: Int32 -> Integer) maxBound) $
  let (w32, g') = SM.bitmaskWithRejection32'
                    ((fromIntegralWrap :: a -> Word32) h) g
      -- `fromIntegralWrap` is fine here, because wrapping is OK.
      x = (fromIntegralWrap :: Word32 -> a) w32
  in if x > h
     then error $ "nextRandom internal error"
                  `showFailure` (toInteger x, toInteger h, w32)
     else (x, g')

-- | Get a random 'Word32' using full range.
randomWord32 :: Rnd Word32
{-# INLINE randomWord32 #-}
randomWord32 = St.state SM.nextWord32

-- | Get any element of a list with equal probability.
oneOf :: [a] -> Rnd a
oneOf [] = error $ "oneOf []" `showFailure` ()
oneOf [x] = return x
oneOf xs = do
  r <- randomR0 (length xs - 1)
  return $! xs !! r

-- | Generates a random permutation. Naive, but good enough for small inputs.
shuffle :: Eq a => [a] -> Rnd [a]
shuffle [] = return []
shuffle l = do
  x <- oneOf l
  (x :) <$> shuffle (delete x l)

-- | Generates a random permutation, except for the existing mapping.
shuffleExcept :: U.Vector Word16 -> Int -> [Word16] -> Rnd [Word16]
shuffleExcept v len l0 = assert (len == length l0) $
  shuffleE 0 (l0 \\ filter (/= maxBound) (U.toList v))
 where
  shuffleE :: Int -> [Word16] -> Rnd [Word16]
  shuffleE i _ | i == len = return []
  shuffleE i l = do
    let a0 = v U.! i
    if a0 == maxBound then do
      a <- oneOf l
      (a :) <$> shuffleE (succ i) (delete a l)
    else
      (a0 :) <$> shuffleE (succ i) l

-- | Gen an element according to a frequency distribution.
frequency :: Show a => Frequency a -> Rnd a
{-# INLINE frequency #-}
frequency = St.state . rollFreq

-- | Randomly choose an item according to the distribution.
rollFreq :: Show a => Frequency a -> SM.SMGen -> (a, SM.SMGen)
rollFreq fr g = case runFrequency fr of
  [] -> error $ "choice from an empty frequency"
                `showFailure` nameFrequency fr
  [(n, x)] | n <= 0 -> error $ "singleton void frequency"
                               `showFailure` (nameFrequency fr, n, x)
  [(_, x)] -> (x, g)  -- speedup
  fs -> let sumf = foldl' (\ !acc (!n, _) -> acc + n) 0 fs
            (r, ng) = nextRandom (pred sumf) g
            frec :: Int -> [(Int, a)] -> a
            frec !m [] = error $ "impossible roll"
                                 `showFailure` (nameFrequency fr, fs, m)
            frec m ((n, x) : _) | m < n = x
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
castDice :: Dice.AbsDepth -> Dice.AbsDepth -> Dice.Dice -> Rnd Int
castDice = Dice.castDice randomR

-- | Cast dice scaled with current level depth and return @True@
-- if the results is greater than 50.
oddsDice :: Dice.AbsDepth -> Dice.AbsDepth -> Dice.Dice -> Rnd Bool
oddsDice ldepth totalDepth dice = do
  c <- castDice ldepth totalDepth dice
  return $! c > 50

-- | Cast dice, scaled with current level depth, for coordinates.
castDiceXY :: Dice.AbsDepth -> Dice.AbsDepth -> Dice.DiceXY -> Rnd (Int, Int)
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
