-- | Custom Prelude, compatible across many GHC versions.
module Game.LambdaHack.Common.Prelude
  ( module Prelude.Compat

  , module Control.Monad.Compat
  , module Data.List.Compat
  , module Data.Maybe
  , module Data.Monoid.Compat

  , module Control.Exception.Assert.Sugar

  , Text, (<+>), tshow, divUp, (<$$>), partitionM, length, null

  , (***), (&&&), first, second
  ) where

import Prelude ()

import Prelude.Compat hiding (appendFile, length, null, readFile, writeFile)

import           Control.Applicative
import           Control.Arrow (first, second, (&&&), (***))
import           Control.Monad.Compat
import           Data.List.Compat hiding (length, null)
import qualified Data.List.Compat as List
import           Data.Maybe
import           Data.Monoid.Compat

import Control.Exception.Assert.Sugar (allB, assert, blame, showFailure, swith)

import Data.Text (Text)

import qualified Data.Text as T (pack)
import           NLP.Miniutter.English ((<+>))

-- | Show and pack the result.
tshow :: Show a => a -> Text
tshow x = T.pack $ show x

infixl 7 `divUp`
-- | Integer division, rounding up.
divUp :: Integral a => a -> a -> a
{-# INLINE divUp #-}
divUp n k = (n + k - 1) `div` k

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
