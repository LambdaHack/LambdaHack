module Assert (assert, blame, allB, failure) where

import Control.Exception (assert)
import Debug.Trace (trace)

infix 1 `blame`
-- | If the condition fails, Display the value blamed for the failure.
-- Used as in "assert (c /= 0 `blame` c) $ 10 / c".
blame :: Show a => Bool -> a -> Bool
{-# INLINE blame #-}
blame condition blamed
  | condition = True
  | otherwise =
    let s = "Contract failed and the following is to blame:\n" ++
            "  " ++ show blamed
    in trace s False

-- | Like List.all, but if the predicate fails, blame all the list elements
-- and especially those for which it fails.
allB :: Show a => (a -> Bool) -> [a] -> Bool
{-# INLINE allB #-}
allB predicate l =
  let s = show (filter (not . predicate) l) ++ " in the context of " ++ show l
  in blame (all predicate l) s

-- | Like Prelude.undefined, but shows the source location
-- and also the value to blame for the failure. To be used as in:
-- assert `failure` ((x1, y1), (x2, y2), "designate a vertical line")
failure :: Show a => (Bool -> b -> b) -> a -> b
{-# INLINE failure #-}
failure f blamed =
  let s = "Internal failure occured and the following is to blame:\n" ++
          "  " ++ show blamed
  in trace s $
     f False (error "Assert.failure: no error location (upgrade to GHC 7.4)")
