-- | Saving/loading to files, with serialization and compression.
module Game.LambdaHack.Common.File
  ( encodeEOF, strictDecodeEOF
  , tryCreateDir, doesFileExist, tryWriteFile, readFile, renameFile
  ) where

import Prelude ()

#ifdef USE_JSFILE
import Game.LambdaHack.Common.JSFile
#elif defined(USE_WASMFILE)
import Game.LambdaHack.Common.WasmFile
#else
import Game.LambdaHack.Common.HSFile
#endif
