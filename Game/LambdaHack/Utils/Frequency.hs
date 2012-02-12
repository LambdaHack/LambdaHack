-- | A list of items with relative frequencies of appearance.
module Game.LambdaHack.Utils.Frequency
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
data Frequency a = Frequency
  { _name        :: String      -- ^ short description for debug, etc.
  , runFrequency :: [(Int, a)]  -- ^ give acces to raw frequency values
  }
  deriving Show

instance Monad Frequency where
  return x = Frequency "return" [(1, x)]
  Frequency name xs >>= f =
    Frequency ("bind (" ++ name ++ ")")
              [(p * q, y) | (p, x) <- xs,
                            (q, y) <- runFrequency (f x) ]

instance MonadPlus Frequency where
  mplus (Frequency xname xs) (Frequency yname ys) =
    Frequency ("mplus (" ++ xname ++ ") (" ++ yname ++ ")")
              (xs ++ ys)
  mzero = Frequency "mzero" []

instance Functor Frequency where
  fmap f (Frequency name xs) = Frequency name (map (\ (p, x) -> (p, f x)) xs)

-- | Uniform discrete frequency distribution.
uniformFreq :: String -> [a] -> Frequency a
uniformFreq name = Frequency name . map (\ x -> (1, x))

-- | Takes a name and a list of frequencies and items
-- into the frequency distribution.
toFreq :: String -> [(Int, a)] -> Frequency a
toFreq = Frequency

-- | Scale frequecy distribution, multiplying it
-- by a positive integer constant.
scaleFreq :: Show a => Int -> Frequency a -> Frequency a
scaleFreq n (Frequency name xs) =
  assert (n > 0 `blame` ("negative scale for " ++ name, n, xs)) $
  Frequency name (map (\ (p, x) -> (n * p, x)) xs)

-- | Leave only items that satisfy a predicate.
filterFreq :: (a -> Bool) -> Frequency a -> Frequency a
filterFreq p (Frequency name l) =
  Frequency ("filterFreq (" ++ name ++ ")")
            (filter (p . snd) l)

-- | Randomly choose an item according to the distribution.
rollFreq :: Show a => Frequency a -> R.StdGen -> (a, R.StdGen)
rollFreq (Frequency name []) _ =
  assert `failure` ("choice from an empty frequency: " ++ name)
rollFreq (Frequency name [(n, x)]) _ | n <= 0 =
  assert `failure` ("singleton frequency with nothing to pick: " ++ name, n, x)
rollFreq (Frequency _ [(_, x)]) g = (x, g)  -- speedup
rollFreq (Frequency name fs) g =
  assert (sumf > 0 `blame` ("frequency with nothing to pick: " ++ name, fs)) $
  (frec r fs, ng)
 where
  sumf = sum (map fst fs)
  (r, ng) = R.randomR (1, sumf) g
  frec :: Int -> [(Int, a)] -> a
  frec m []                     = assert `failure` ("impossible", name, fs, m)
  frec m ((n, x) : _)  | m <= n = x
  frec m ((n, _) : xs)          = frec (m - n) xs

-- | Test if the frequency distribution is empty.
nullFreq :: Frequency a -> Bool
nullFreq fr = null $ runFrequency fr
