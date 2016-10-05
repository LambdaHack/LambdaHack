-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Server.FileM
  ( encodeEOF, strictDecodeEOF
  , tryCreateDir, doesFileExist, tryWriteFile, readFile
  ) where

import Prelude ()

#ifdef USE_JSFILE_SERVER
import Game.LambdaHack.Common.JSFile
#else
import Game.LambdaHack.Common.HSFile
#endif
