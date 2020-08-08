{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Custom Prelude, compatible across many GHC versions.
module Game.LambdaHack.Core.Prelude
  ( module Prelude.Compat

  , module Control.Monad.Compat
  , module Data.List.Compat
  , module Data.Maybe
  , module Data.Semigroup.Compat

  , module Control.Exception.Assert.Sugar

  , Text, (<+>), tshow, divUp, sum, (<$$>), partitionM, length, null, comparing
  , fromIntegralTypeMe, intToDouble, intToInt64

  , (***), (&&&), first, second
  ) where

import Prelude ()

import Prelude.Compat hiding
  ( appendFile
  , foldl
  , foldl1
  , fromIntegral
  , length
  , null
  , readFile
  , sum
  , writeFile
  , (<>)
  )

import           Control.Applicative
import           Control.Arrow (first, second, (&&&), (***))
import           Control.DeepSeq
import           Control.Exception.Assert.Sugar
  (allB, assert, blame, showFailure, swith)
import           Control.Monad.Compat
import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Fixed as Fixed
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Key
import           Data.List.Compat hiding (foldl, foldl1, length, null, sum)
import qualified Data.List.Compat as List
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Semigroup.Compat (Semigroup ((<>)))
import           Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Time as Time
import           NLP.Miniutter.English ((<+>))
import qualified NLP.Miniutter.English as MU
import qualified Prelude.Compat

-- | Show and pack the result.
tshow :: Show a => a -> Text
tshow x = T.pack $ show x

infixl 7 `divUp`
-- | Integer division, rounding up.
divUp :: Integral a => a -> a -> a
{-# INLINE divUp #-}
divUp n k = (n + k - 1) `div` k

sum :: Num a => [a] -> a
sum = foldl' (+) 0

infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
h <$$> m = fmap h <$> m

partitionM :: Applicative m => (a -> m Bool) -> [a] -> m ([a], [a])
{-# INLINE partitionM #-}
partitionM p = foldr (\a ->
  liftA2 (\b -> (if b then first else second) (a :)) (p a)) (pure ([], []))

-- | A version specialized to lists to avoid errors such as taking length
-- of @Maybe [a]@ instead of @[a]@.
-- Such errors are hard to detect, because the type of elements of the list
-- is not constrained.
length :: [a] -> Int
length = List.length

-- | A version specialized to lists to avoid errors such as taking null
-- of @Maybe [a]@ instead of @[a]@.
-- Such errors are hard to detect, because the type of elements of the list
-- is not constrained.
null :: [a] -> Bool
null = List.null

-- Data.Binary orphan instances

instance (Enum k, Binary k, Binary e) => Binary (EM.EnumMap k e) where
  put m = put (EM.size m) >> mapM_ put (EM.toAscList m)
  get = EM.fromDistinctAscList <$> get

instance (Enum k, Binary k) => Binary (ES.EnumSet k) where
  put m = put (ES.size m) >> mapM_ put (ES.toAscList m)
  get = ES.fromDistinctAscList <$> get

instance Binary Time.NominalDiffTime where
  get = fmap realToFrac (get :: Get Fixed.Pico)
  put = (put :: Fixed.Pico -> Put) . realToFrac

instance (Hashable k, Eq k, Binary k, Binary v) => Binary (HM.HashMap k v) where
  get = fmap HM.fromList get
  put = put . HM.toList

-- Data.Key orphan instances

type instance Key (EM.EnumMap k) = k

instance Zip (EM.EnumMap k) where
  {-# INLINE zipWith #-}
  zipWith = EM.intersectionWith

instance Enum k => ZipWithKey (EM.EnumMap k) where
  {-# INLINE zipWithKey #-}
  zipWithKey = EM.intersectionWithKey

instance Enum k => Keyed (EM.EnumMap k) where
  {-# INLINE mapWithKey #-}
  mapWithKey = EM.mapWithKey

instance Enum k => FoldableWithKey (EM.EnumMap k) where
  {-# INLINE foldrWithKey #-}
  foldrWithKey = EM.foldrWithKey

instance Enum k => TraversableWithKey (EM.EnumMap k) where
  traverseWithKey f = fmap EM.fromDistinctAscList
                      . traverse (\(k, v) -> (,) k <$> f k v) . EM.toAscList

instance Enum k => Indexable (EM.EnumMap k) where
  {-# INLINE index #-}
  index = (EM.!)

instance Enum k => Lookup (EM.EnumMap k) where
  {-# INLINE lookup #-}
  lookup = EM.lookup

instance Enum k => Adjustable (EM.EnumMap k) where
  {-# INLINE adjust #-}
  adjust = EM.adjust

-- Data.Hashable orphan instances

instance (Enum k, Hashable k, Hashable e) => Hashable (EM.EnumMap k e) where
  hashWithSalt s x = hashWithSalt s (EM.toAscList x)

instance (Enum k, Hashable k) => Hashable (ES.EnumSet k) where
  hashWithSalt s x = hashWithSalt s (ES.toAscList x)

-- Control.DeepSeq orphan instances

instance NFData MU.Part

instance NFData MU.Person

instance NFData MU.Polarity

-- | Re-exported 'Prelude.fromIntegral', but please give it explicit type
-- to make it obvious if wrapping, etc., may occur.
fromIntegralTypeMe :: (Integral a, Num b) => a -> b
fromIntegralTypeMe = Prelude.Compat.fromIntegral

intToDouble :: Int -> Double
intToDouble = Prelude.Compat.fromIntegral

intToInt64 :: Int -> Int64
intToInt64 = Prelude.Compat.fromIntegral
