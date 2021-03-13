{-# LANGUAGE DeriveGeneric, DeriveTraversable, TupleSections #-}
-- | A list of entities with relative frequencies of appearance.
module Game.LambdaHack.Core.Frequency
  ( -- * The @Frequency@ type
    Frequency
    -- * Construction
  , uniformFreq, toFreq, maxBoundInt32
    -- * Transformation
  , scaleFreq
    -- * Consumption
  , nullFreq, runFrequency, nameFrequency
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Control.Applicative
import Data.Int (Int32)
import GHC.Generics (Generic)

maxBoundInt32 :: Int
maxBoundInt32 = toIntegralCrash (maxBound :: Int32)

-- | The frequency distribution type. Not normalized (operations may
-- or may not group the same elements and sum their frequencies).
-- However, elements with zero frequency are removed upon construction.
--
-- The @Eq@ instance compares raw representations, not relative,
-- normalized frequencies, so operations don't need to preserve
-- the expected equalities.
data Frequency a = Frequency
  { runFrequency  :: [(Int, a)]  -- ^ give acces to raw frequency values
  , nameFrequency :: Text        -- ^ short description for debug, etc.
  }
  deriving (Show, Eq, Ord, Foldable, Traversable, Generic)

instance Monad Frequency where
  Frequency xs name >>= f =
    Frequency [
#ifdef WITH_EXPENSIVE_ASSERTIONS
                assert (toInteger p * toInteger q <= toInteger maxBoundInt32
                        `blame` (name, map fst xs))
#endif
                (p * q, y)
              | (p, x) <- xs
              , (q, y) <- runFrequency (f x)
              ]
              ("bind (" <> name <> ")")

instance Functor Frequency where
  fmap f (Frequency xs name) = Frequency (map (second f) xs) name

instance Applicative Frequency where
  {-# INLINE pure #-}
  pure x = Frequency [(1, x)] "pure"
  Frequency fs fname <*> Frequency ys yname =
    Frequency [
#ifdef WITH_EXPENSIVE_ASSERTIONS
                assert (toInteger p * toInteger q <= toInteger maxBoundInt32
                        `blame` (fname, map fst fs, yname, map fst ys))
#endif
                (p * q, f y)
              | (p, f) <- fs
              , (q, y) <- ys
              ]
              ("(" <> fname <> ") <*> (" <> yname <> ")")

instance MonadPlus Frequency where
  mplus (Frequency xs xname) (Frequency ys yname) =
    let name = case (xs, ys) of
          ([], []) -> "[]"
          ([], _) -> yname
          (_, []) -> xname
          _ -> "(" <> xname <> ") ++ (" <> yname <> ")"
    in Frequency (xs ++ ys) name
  mzero = Frequency [] "[]"

instance Alternative Frequency where
  (<|>) = mplus
  empty = mzero

-- | Uniform discrete frequency distribution.
uniformFreq :: Text -> [a] -> Frequency a
uniformFreq name l = Frequency (map (1,) l) name

-- | Takes a name and a list of frequencies and items
-- into the frequency distribution.
toFreq :: Text -> [(Int, a)] -> Frequency a
toFreq name l =
#ifdef WITH_EXPENSIVE_ASSERTIONS
  assert (all (\(p, _) -> toInteger p <= toInteger maxBoundInt32) l
          `blame` (name, map fst l)) $
#endif
  Frequency (filter ((> 0 ) . fst) l) name

-- | Scale frequency distribution, multiplying it
-- by a positive integer constant.
scaleFreq :: Show a => Int -> Frequency a -> Frequency a
scaleFreq n (Frequency xs name) =
  assert (n > 0 `blame` "non-positive frequency scale" `swith` (name, n, xs)) $
  let multN p =
#ifdef WITH_EXPENSIVE_ASSERTIONS
                assert (toInteger p * toInteger n <= toInteger maxBoundInt32
                        `blame` (n, Frequency xs name)) $
#endif
                p * n
  in Frequency (map (first multN) xs) name

-- | Test if the frequency distribution is empty.
nullFreq :: Frequency a -> Bool
nullFreq (Frequency fs _) = null fs
