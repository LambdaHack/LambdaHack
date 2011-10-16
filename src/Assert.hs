module Assert (assert, blame) where

import Control.Exception (assert)
import System.IO
import System.IO.Unsafe

blame :: Show a => Bool -> a -> Bool
blame condition culprit
  | condition = True
  | otherwise =
    let action = do
          hPutStrLn stderr "Internal error, possibly due to the following:"
          hPutStrLn stderr ("  " ++ show culprit)
    in unsafePerformIO action `seq` False
