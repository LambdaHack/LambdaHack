{-# LANGUAGE RankNTypes #-}
-- | Syntactic sugar that improves the usability of 'Control.Exception.assert'
-- and 'error'.
--
-- This is actually a bunch of hacks wrapping the original @assert@ function,
-- which is, as of GHC 7.8, the only simple way of obtaining source positions.
-- The original @assert@ function is re-exported for convenience.
--
-- Make sure to enable assertions for your cabal package, e.g., by setting
--
-- > ghc-options: -fno-ignore-asserts
--
-- in your .cabal file. Otherwise, some of the functions will have
-- no effect at all.
module Game.LambdaHack.Utils.Assert
  ( assert, blame, failure, with, swith, allB, skip, forceEither
  ) where

import Control.Exception (assert)
import Data.Text (Text)
import Debug.Trace (trace)
import qualified Text.Show.Pretty as Show.Pretty (ppShow)

infix 1 `blame`
-- | If the condition fails, display the value blamed for the failure.
-- Used as in
--
-- > assert (age >= 0 `blame` age) $ savings / (99 - age)
blame :: Show a => Bool -> a -> Bool
{-# INLINE blame #-}
blame True _ = True
blame False blamed = trace (blameMessage blamed) False

blameMessage :: Show a => a -> String
{-# NOINLINE blameMessage #-}
blameMessage blamed = "Contract failed and the following is to blame:\n  "
                      ++ Show.Pretty.ppShow blamed

infix 1 `failure`
-- | Like 'error', but shows the source position and also
-- the value to blame for the failure. To be used as in
--
-- > case xs of
-- >   0 : _ -> assert `failure` (xs, "has an insignificant zero")
failure :: Show a => (forall x. Bool -> x -> x) -> a -> b
{-# NOINLINE failure #-}
failure asrt blamed =
  let s = "Internal failure occurred and the following is to blame:\n  "
          ++ Show.Pretty.ppShow blamed
  in trace s
     $ asrt False
     $ error "Assert.failure: no error position (upgrade to GHC >= 7.4)"

infix 2 `with`
-- | Syntactic sugar for the pair operation, to be used in 'blame'
-- and 'failure' as in
--
-- > assert (age >= 0 `blame` "negative age" `with` age) $ savings / (99 - age)
--
-- or
--
-- > case xs of
-- >   0 : _ -> assert `failure` "insignificant zero" `with` xs
--
-- Fixing the first component of the pair to @Text@ prevents warnings
-- about defaulting.
with :: Text -> b -> (Text, b)
{-# INLINE with #-}
with t b = (t, b)

infix 2 `swith`
-- | Syntactic sugar for the pair operation, to be used in 'blame'
-- and 'failure' as in
--
-- > assert (age >= 0 `blame` "negative age" `swith` age) $ savings / (99 - age)
--
-- or
--
-- > case xs of
-- >   0 : _ -> assert `failure` "insignificant zero" `swith` xs
--
-- Fixing the first component of the pair to @String@ prevents warnings
-- about defaulting.
swith :: String -> b -> (String, b)
{-# INLINE swith #-}
swith t b = (t, b)

-- | Like 'List.all', but if the predicate fails, blame all the list elements
-- and especially those for which it fails. To be used as in
--
-- > assert (allB (<= height) [yf, y1, y2])
allB :: Show a => (a -> Bool) -> [a] -> Bool
{-# INLINE allB #-}
allB predicate l = blame (all predicate l) $ allBMessage predicate l

allBMessage :: Show a => (a -> Bool) -> [a] -> String
{-# NOINLINE allBMessage #-}
allBMessage predicate l = Show.Pretty.ppShow (filter (not . predicate) l)
                          ++ " in the context of "
                          ++ Show.Pretty.ppShow l

-- | To be used in place of the verbose @(return ())@, as in
--
-- > do k <- getK7 r
-- >    assert (k <= maxK `blame` "K7 too large" `with` r) skip
-- >    return $ k >= averageK
skip :: Monad m => m ()
{-# INLINE skip #-}
skip = return ()

infix 1 `forceEither`
-- | Assuming that @Left@ signifies an error condition,
-- check the @Either@ value and, if @Left@ is encountered,
-- fail outright and show the error message. Used as in
--
-- > assert `forceEither` parseOrFailWithMessage code
forceEither :: Show a => (forall x. Bool -> x -> x) -> Either a b -> b
{-# NOINLINE forceEither #-}
forceEither asrt (Left a)  = asrt `failure` a
forceEither _    (Right b) = b
