-- | A list of items with relative frequencies of appearance.
module Game.LambdaHack.Frequency
  ( -- * The @Frequency@ type
    Frequency
    -- * Construction
  , uniformFreq, toFreq
    -- * Transformation
  , scaleFreq, filterFreq
    -- * Consumption
  , rollFreq, nullFreq, runFrequency
  ) where

import Control.Monad
import qualified System.Random as R

import Game.LambdaHack.Utils.Assert

-- TODO: do not expose runFrequency
-- | The frequency distribution type.
newtype Frequency a = Frequency { runFrequency :: [(Int, a)] }
  deriving Show

instance Monad Frequency where
  return x = Frequency [(1, x)]
  m >>= f  = Frequency
               [(p * q, y) | (p, x) <- runFrequency m,
                             (q, y) <- runFrequency (f x) ]

instance MonadPlus Frequency where
  mplus (Frequency xs) (Frequency ys) = Frequency (xs ++ ys)
  mzero = Frequency []

instance Functor Frequency where
  fmap f (Frequency xs) = Frequency (map (\ (p, x) -> (p, f x)) xs)

-- | Uniform discrete frequency distribution.
uniformFreq :: [a] -> Frequency a
uniformFreq = Frequency . map (\ x -> (1, x))

-- | Takes a list of frequencies and items into the frequency distribution.
toFreq :: [(Int, a)] -> Frequency a
toFreq = Frequency

-- | Scale frequecy distribution, multiplying it by an integer constant.
scaleFreq :: Int -> Frequency a -> Frequency a
scaleFreq n (Frequency xs) = Frequency (map (\ (p, x) -> (n * p, x)) xs)

-- | Leave only items that satisfy a predicate.
filterFreq :: (a -> Bool) -> Frequency a -> Frequency a
filterFreq p (Frequency l) = Frequency $ filter (p . snd) l

-- | Randomly choose an item according to the distribution.
rollFreq :: Show a => Frequency a -> R.StdGen -> (a, R.StdGen)
rollFreq (Frequency []) _ =
  assert `failure` "choice from an empty frequency"
rollFreq (Frequency [(n, x)]) _ | n <= 0 =
  assert `failure` ("singleton frequency with nothing to pick", n, x)
rollFreq (Frequency [(_, x)]) g = (x, g)  -- speedup
rollFreq (Frequency fs) g =
  assert (sumf > 0 `blame` ("frequency with nothing to pick", fs)) $
  (frec r fs, ng)
 where
  sumf = sum (map fst fs)
  (r, ng) = R.randomR (1, sumf) g
  frec :: Int -> [(Int, a)] -> a
  frec m []                     = assert `failure` ("impossible", fs, m)
  frec m ((n, x) : _)  | m <= n = x
  frec m ((n, _) : xs)          = frec (m - n) xs

-- | Test if the frequency distribution is empty.
nullFreq :: Frequency a -> Bool
nullFreq fr = null $ runFrequency fr
