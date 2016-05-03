-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Common.Prelude
  ( module Prelude.Compat

  , module Control.Monad.Compat
  , module Data.List.Compat
  , module Data.Maybe
  , module Data.Monoid.Compat

  , module Control.Exception.Assert.Sugar

  , Text, (<+>), tshow, divUp
  ) where

import Prelude ()

import Prelude.Compat

import Control.Monad.Compat
import Data.List.Compat
import Data.Maybe
import Data.Monoid.Compat

import Control.Exception.Assert.Sugar

import Data.Text (Text)

import qualified Data.Text as T (pack)
import qualified NLP.Miniutter.English as MU ((<+>))

infixr 6 <+>  -- TODO: not needed when we require a very new minimorph
(<+>) :: Text -> Text -> Text
(<+>) = (MU.<+>)

-- | Show and pack the result.
tshow :: Show a => a -> Text
tshow x = T.pack $ show x

infixl 7 `divUp`
-- | Integer division, rounding up.
divUp :: Integral a => a -> a -> a
{-# INLINE divUp #-}
divUp n k = (n + k - 1) `div` k
