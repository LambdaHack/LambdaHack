module Assert (assert, blame) where

import Control.Exception (assert)
import System.IO
import System.IO.Unsafe

-- | If the condition fails, Display the value blamed for the failure.
-- Used as in "assert (c /= 0 `blame` c) $ 10 / c".
blame :: Show a => Bool -> a -> Bool
blame condition blamed
  | condition = True
  | otherwise =
    let action = do
          hPutStrLn stderr
            "Internal error occurred and the following is to blame:"
          hPutStrLn stderr ("  " ++ show blamed)
    in unsafePerformIO action `seq` False

-- | Assert that displays the value to blame for the failure.
-- The INLINE pragme serves to show the source location of the call site,
-- not of this definition. Unfortunately, it does not work.
assertB :: Show a => Bool -> a -> b -> b
{-# INLINE assertB #-}
assertB condition blamed rest =
  assert (condition `blame` blamed) rest

-- | Like Prelude.undefined, but shows the source location.
-- Except that it does not work.
undef :: b
{-# INLINE undef #-}
undef = assert False (error "Internal error inside Assert.undef")

-- | Like undef, but also displays the value to blame for the failure.
undefB :: Show a => a -> b
{-# INLINE undefB #-}
undefB blamed =
  assert (False `blame` blamed) (error "Internal error inside Assert.undefB")
