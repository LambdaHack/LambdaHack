-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Client.FileM
  ( encodeEOF, strictDecodeEOF
  , tryCreateDir, doesFileExist, tryWriteFile, readFile
  ) where

import Prelude ()

#ifdef USE_JSFILE_CLIENT
import Game.LambdaHack.Common.JSFile
#else
import Game.LambdaHack.Common.HSFile
#endif
