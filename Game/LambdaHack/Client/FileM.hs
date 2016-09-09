-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Client.FileM
  ( encodeEOF, strictDecodeEOF, tryCreateDir, tryCopyDataFiles, appDataDir
  , fileOperationImplemented
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

#ifdef USE_JSFILE_CLIENT

import Game.LambdaHack.Common.JSFile

fileOperationImplemented :: Bool
fileOperationImplemented = False

#else

import Game.LambdaHack.Common.HSFile

fileOperationImplemented :: Bool
fileOperationImplemented = True

#endif
