-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Common.File
  ( encodeEOF, strictDecodeEOF
  , tryCreateDir, doesFileExist, tryWriteFile, readFile
  ) where

import Prelude ()

#ifdef USE_JSFILE
import Game.LambdaHack.Common.JSFile
#else
import Game.LambdaHack.Common.HSFile
#endif
