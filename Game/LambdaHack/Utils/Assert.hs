-- | Tools for specifying assertions. A step towards contracts.
-- Actually, a bunch of hacks wrapping the original @assert@ function,
-- which is the only easy way of obtaining source positions.
module Game.LambdaHack.Utils.Assert
  ( assert, blame, failure, allB, skip
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
-- | Like 'error', but shows the source position and also
-- the value to blame for the failure. To be used as in:
--
-- > assert `failure` ((x1, y1), (x2, y2), "designate a vertical line")
failure :: Show a => (Bool -> b -> b) -> a -> b
{-# INLINE failure #-}
failure asrt blamed =
  let s = "Internal failure occured and the following is to blame:\n" ++
          "  " ++ show blamed
  in trace s $
     asrt False
       (error "Assert.failure: no error position (upgrade to GHC >= 7.4)")

-- | Like 'List.all', but if the predicate fails, blame all the list elements
-- and especially those for which it fails. To be used as in:
--
-- > assert (allB (>= 0) [yf, xf, y1, x1, y2, x2])
allB :: Show a => (a -> Bool) -> [a] -> Bool
{-# INLINE allB #-}
allB predicate l =
  let s = show (filter (not . predicate) l) ++ " in the context of " ++ show l
  in blame (all predicate l) s

-- | To be used in place of the verbose @skip@, as in:
--
-- > do b <- getB a
-- >    assert (b `blame` a) skip
skip :: Monad m => m ()
skip = return ()
