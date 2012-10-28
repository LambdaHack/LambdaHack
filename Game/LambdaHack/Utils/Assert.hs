-- | Tools for specifying assertions. A step towards contracts.
-- Actually, a bunch of hacks wrapping the original @assert@ function,
-- which is the only easy way of obtaining source locations.
module Game.LambdaHack.Utils.Assert
  ( assert, blame, failure, allB, checkM, trueM, falseM
  ) where

import Control.Exception (assert)
import Debug.Trace (trace)

infix 1 `blame`
-- | If the condition fails, display the value blamed for the failure.
-- Used as in
--
-- > assert (c /= 0 `blame` c) $ 10 / c
blame :: Show a => Bool -> a -> Bool
{-# INLINE blame #-}
blame condition blamed
  | condition = True
  | otherwise =
    let s = "Contract failed and the following is to blame:\n" ++
            "  " ++ show blamed
    in trace s False

infix 1 `failure`
-- | Like 'Prelude.undefined', but shows the source location
-- and also the value to blame for the failure. To be used as in:
--
-- > assert `failure` ((x1, y1), (x2, y2), "designate a vertical line")
failure :: Show a => (Bool -> b -> b) -> a -> b
{-# INLINE failure #-}
failure asrt blamed =
  let s = "Internal failure occured and the following is to blame:\n" ++
          "  " ++ show blamed
  in trace s $
     asrt False
       (error "Assert.failure: no error location (upgrade to GHC >= 7.4)")

-- | Like 'List.all', but if the predicate fails, blame all the list elements
-- and especially those for which it fails. To be used as in:
--
-- > assert (allB (>= 0) [yf, xf, y1, x1, y2, x2])
allB :: Show a => (a -> Bool) -> [a] -> Bool
{-# INLINE allB #-}
allB predicate l =
  let s = show (filter (not . predicate) l) ++ " in the context of " ++ show l
  in blame (all predicate l) s

-- | Check that the value returned from a monad action satisfies a predicate.
-- Reports source location and the suspects. Drops the value.
checkM :: (Show a, Monad m) =>
          (Bool -> m () -> m ()) -> (c -> Bool) -> a -> c -> m ()
checkM asrt predicate blamed value
  | predicate value = return ()
  | otherwise =
    let s = "The returned value is wrong and the following is to blame:\n" ++
            "  " ++ show blamed
    in trace s $
       asrt False
         (error "Assert.checkM: no error location (upgrade to GHC >= 7.4)")

-- | Verifies that the returned value is true (respectively, false). Used as in:
--
-- > open newValve >>= assert `trueM` (newValve, "is already opened, not new")
trueM, falseM :: (Show a, Monad m) => (Bool -> m () -> m ()) -> a -> Bool
              -> m ()
trueM  asrt = checkM asrt id
falseM asrt = checkM asrt not
