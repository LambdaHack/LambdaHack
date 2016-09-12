{-# LANGUAGE DeriveFoldable, DeriveGeneric, DeriveTraversable #-}
-- | A list of items with relative frequencies of appearance.
module Game.LambdaHack.Common.Frequency
  ( -- * The @Frequency@ type
    Frequency
    -- * Construction
  , uniformFreq, toFreq
    -- * Transformation
  , scaleFreq, renameFreq, setFreq
    -- * Consumption
  , nullFreq, runFrequency, nameFrequency
  , maxFreq, minFreq, meanFreq
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Applicative
import Control.Arrow (first, second)
import Control.DeepSeq
import Data.Binary
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- TODO: do not expose runFrequency
-- | The frequency distribution type. Not normalized (operations may
-- or may not group the same elements and sum their frequencies).
-- However, elements with zero frequency are removed upon construction.
--
-- The @Eq@ instance compares raw representations, not relative,
-- normalized frequencies, so operations don't need to preserve
-- the expected equalities, unless they do some kind of normalization
-- (see 'Dice').
data Frequency a = Frequency
  { runFrequency  :: ![(Int, a)]  -- ^ give acces to raw frequency values
  , nameFrequency :: Text         -- ^ short description for debug, etc.;
                                  --   keep it lazy, because it's rarely used
  }
  deriving (Show, Eq, Ord, Foldable, Traversable, Generic)

instance Monad Frequency where
  {-# INLINE return #-}
  return x = Frequency [(1, x)] "return"
  Frequency xs name >>= f =
    Frequency [ (p * q, y) | (p, x) <- xs
                           , (q, y) <- runFrequency (f x) ]
              ("bind (" <> name <> ")")

instance Functor Frequency where
  fmap f (Frequency xs name) = Frequency (map (second f) xs) name

instance Applicative Frequency where
  pure  = return
  Frequency fs fname <*> Frequency ys yname =
    Frequency [ (p * q, f y) | (p, f) <- fs
                             , (q, y) <- ys ]
              ("(" <> fname <> ") <*> (" <> yname <> ")")

instance MonadPlus Frequency where
  mplus (Frequency xs xname) (Frequency ys yname) =
    let name = case (xs, ys) of
          ([], []) -> "[]"
          ([], _ ) -> yname
          (_,  []) -> xname
          _ -> "(" <> xname <> ") ++ (" <> yname <> ")"
    in Frequency (xs ++ ys) name
  mzero = Frequency [] "[]"

instance Alternative Frequency where
  (<|>) = mplus
  empty = mzero

instance Hashable a => Hashable (Frequency a)

instance Binary a => Binary (Frequency a)

instance NFData a => NFData (Frequency a)

-- | Uniform discrete frequency distribution.
uniformFreq :: Text -> [a] -> Frequency a
uniformFreq name l = Frequency (map (\x -> (1, x)) l) name

-- | Takes a name and a list of frequencies and items
-- into the frequency distribution.
toFreq :: Text -> [(Int, a)] -> Frequency a
toFreq name l = Frequency (filter ((> 0 ) . fst) l) name

-- | Scale frequency distribution, multiplying it
-- by a positive integer constant.
scaleFreq :: Show a => Int -> Frequency a -> Frequency a
scaleFreq n (Frequency xs name) =
  assert (n > 0 `blame` "non-positive frequency scale" `twith` (name, n, xs)) $
  Frequency (map (first (* n)) xs) name

-- | Change the description of the frequency.
renameFreq :: Text -> Frequency a -> Frequency a
renameFreq newName fr = fr {nameFrequency = newName}

-- | Set frequency of an element.
setFreq :: Eq a => Frequency a -> a -> Int -> Frequency a
setFreq (Frequency xs name) x n =
  let xsNew = [(n, x) | n <= 0] ++ filter ((/= x) . snd) xs
  in Frequency xsNew name

-- | Test if the frequency distribution is empty.
nullFreq :: Frequency a -> Bool
{-# INLINABLE nullFreq #-}
nullFreq (Frequency fs _) = null fs

maxFreq :: Ord a => Frequency a -> Maybe a
{-# INLINABLE maxFreq #-}
maxFreq fr = if nullFreq fr then Nothing else Just $ maximum fr

minFreq :: Ord a => Frequency a -> Maybe a
{-# INLINABLE minFreq #-}
minFreq fr = if nullFreq fr then Nothing else Just $ minimum fr

-- | Average value of an @Int@ distribution, rounded up to avoid truncating
-- it in the other code higher up, which would equate 1d0 with 1d1.
meanFreq :: Frequency Int -> Int
{-# INLINABLE meanFreq #-}
meanFreq fr@(Frequency xs _) = case xs of
  [] -> assert `failure` fr
  _ -> let sumX = sum [ p * x | (p, x) <- xs ]
           sumP = sum $ map fst xs
       in sumX `divUp` sumP
