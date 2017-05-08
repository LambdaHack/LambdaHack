-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Common.Prelude
  ( module Prelude.Compat

  , module Control.Monad.Compat
  , module Data.List.Compat
  , module Data.Maybe
  , module Data.Monoid.Compat

  , module Control.Exception.Assert.Sugar

  , Text, (<+>), tshow, divUp, (<$$>), partitionM

  , (***), (&&&), first, second
  ) where

import Prelude ()

import Prelude.Compat hiding (appendFile, readFile, writeFile)

import Control.Applicative
import Control.Arrow (first, second, (&&&), (***))
import Control.Monad.Compat
import Data.List.Compat
import Data.Maybe
import Data.Monoid.Compat

import Control.Exception.Assert.Sugar

import Data.Text (Text)

import qualified Data.Text as T (pack)
import NLP.Miniutter.English ((<+>))

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
