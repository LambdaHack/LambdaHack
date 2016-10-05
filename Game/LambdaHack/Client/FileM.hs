-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Client.FileM
  ( encodeEOF, strictDecodeEOF
  , tryCreateDir, doesFileExist, tryWriteFile, readFile
  , fileOperationImplemented
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

#ifdef USE_JSFILE_CLIENT

import Game.LambdaHack.Common.JSFile

-- TODO: enable unconditionally if the speed and the 5MB limit proves acceptable
fileOperationImplemented :: Bool
fileOperationImplemented = True

#else

import Game.LambdaHack.Common.HSFile

fileOperationImplemented :: Bool
fileOperationImplemented = True

#endif
