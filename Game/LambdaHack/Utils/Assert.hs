-- | Tools for specifying assertions. A step towards contracts.
-- Actually, a bunch of hacks wrapping the original @assert@ function,
-- which is the only easy way of obtaining source positions.
module Game.LambdaHack.Utils.Assert
  ( assert, blame, with, failure, allB, skip, forceEither
  ) where

import Control.Exception (assert)
import Data.Text (Text)
import Debug.Trace (trace)
import qualified Text.Show.Pretty as Show.Pretty

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
            "  " ++ Show.Pretty.ppShow blamed
    in trace s False

infix 2 `with`
with :: Text -> b -> (Text, b)
with t b = (t, b)

infix 1 `failure`
-- | Like 'error', but shows the source position and also
-- the value to blame for the failure. To be used as in:
--
-- > assert `failure` ((x1, y1), (x2, y2), "designate a vertical line")
failure :: Show a => (Bool -> b -> b) -> a -> b
{-# INLINE failure #-}
failure asrt blamed =
  let s = "Internal failure occured and the following is to blame:\n" ++
          "  " ++ Show.Pretty.ppShow blamed
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
  let s = Show.Pretty.ppShow (filter (not . predicate) l)
          ++ " in the context of "
          ++ Show.Pretty.ppShow l
  in blame (all predicate l) s

-- | To be used in place of the verbose @skip@, as in:
--
-- > do b <- getB a
-- >    assert (b `blame` a) skip
skip :: Monad m => m ()
skip = return ()

-- | In case of corruption, just fail and show the error message.
forceEither :: Show a => Either a b -> b
forceEither (Left a)  = assert `failure` "unexpected Left" `with` a
forceEither (Right b) = b
