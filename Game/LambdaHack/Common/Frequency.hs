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

import Control.Applicative
import Control.Arrow (first, second)
import Control.Exception.Assert.Sugar
import Control.Monad
import Data.Binary
import Data.Foldable (Foldable)
import qualified Data.Hashable as Hashable
import Data.Ratio
import Data.Text (Text)
import Data.Traversable (Traversable)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Msg

-- TODO: do not expose runFrequency
-- | The frequency distribution type. Not normalized (operations may
-- or may not group the same elements and sum their frequencies).
--
-- The @Eq@ instance compares raw representations, not relative,
-- normalized frequencies, so operations don't need to preserve
-- the expected equalities, unless they do some kind of normalization
-- (see 'Dice').
data Frequency a = Frequency
  { nameFrequency :: Text         -- ^ short description for debug, etc.;
                                  --   keep it lazy, because it's rarely used
  , runFrequency  :: ![(Int, a)]  -- ^ give acces to raw frequency values
  }
  deriving (Show, Read, Eq, Ord, Foldable, Traversable, Generic)

instance Monad Frequency where
  {-# INLINE return #-}
  return x = Frequency "return" [(1, x)]
  Frequency name xs >>= f =
    Frequency ("bind (" <> name <> ")")
              [ (p * q, y) | (p, x) <- xs
                           , (q, y) <- runFrequency (f x) ]

instance Functor Frequency where
  fmap f (Frequency name xs) = Frequency name (map (second f) xs)

instance Applicative Frequency where
  pure  = return
  Frequency fname fs <*> Frequency yname ys =
    Frequency ("(" <> fname <> ") <*> (" <> yname <> ")")
              [ (p * q, f y) | (p, f) <- fs
                             , (q, y) <- ys ]

instance MonadPlus Frequency where
  mplus (Frequency xname xs) (Frequency yname ys) =
    let name = case (xs, ys) of
          ([], []) -> "[]"
          ([], _ ) -> yname
          (_,  []) -> xname
          _ -> "(" <> xname <> ") ++ (" <> yname <> ")"
    in Frequency name (xs ++ ys)
  mzero = Frequency "[]" []

instance Alternative Frequency where
  (<|>) = mplus
  empty = mzero

instance Hashable.Hashable a => Hashable.Hashable (Frequency a)

instance Binary a => Binary (Frequency a)

-- | Uniform discrete frequency distribution.
uniformFreq :: Text -> [a] -> Frequency a
uniformFreq name = Frequency name . map (\x -> (1, x))

-- | Takes a name and a list of frequencies and items
-- into the frequency distribution.
toFreq :: Text -> [(Int, a)] -> Frequency a
toFreq = Frequency

-- | Scale frequecy distribution, multiplying it
-- by a positive integer constant.
scaleFreq :: Show a => Int -> Frequency a -> Frequency a
scaleFreq n (Frequency name xs) =
  assert (n > 0 `blame` "non-positive frequency scale" `twith` (name, n, xs)) $
  Frequency name (map (first (* n)) xs)

-- | Change the description of the frequency.
renameFreq :: Text -> Frequency a -> Frequency a
renameFreq newName fr = fr {nameFrequency = newName}

-- | Set frequency of an element.
setFreq :: Eq a => Frequency a -> a -> Int -> Frequency a
setFreq (Frequency name xs) x n =
  let f (_, y) | y == x = (n, x)
      f my = my
  in Frequency name $ map f xs

-- | Test if the frequency distribution is empty.
nullFreq :: Frequency a -> Bool
nullFreq (Frequency _ fs) = all (<= 0) $ map fst fs

maxFreq :: (Show a, Ord a) => Frequency a -> a
maxFreq fr@(Frequency _ xs) = case filter ((> 0 ) . fst) xs of
  [] -> assert `failure` fr
  ys -> maximum $ map snd ys

minFreq :: (Show a, Ord a) => Frequency a -> a
minFreq fr@(Frequency _ xs) = case filter ((> 0 ) . fst) xs of
  [] -> assert `failure` fr
  ys -> minimum $ map snd ys

meanFreq :: (Show a, Integral a) => Frequency a -> Rational
meanFreq fr@(Frequency _ xs) = case filter ((> 0 ) . fst) xs of
  [] -> assert `failure` fr
  ys -> let sumP = sum $ map fst ys
            sumX = sum [ fromIntegral p * x | (p, x) <- ys ]
        in if sumX == 0 then 0 else fromIntegral sumP % fromIntegral sumX
