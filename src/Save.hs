module Save where

import System.Directory
import Control.Exception as E hiding (handle)

import File
import Level
import State
import qualified Config

-- | Name of the save game.
file :: Config.CP -> IO FilePath
file config = Config.getFile config "files" "saveGame"

-- | We save a simple serialized version of the current level and
-- the current state. The 'False' is used only as an EOF marker.
saveGame :: State -> IO ()
saveGame state =
  do
    f <- file (sconfig state)
    encodeCompressedFile f (state, False)

-- | Restore a saved game. Returns either the current level and
-- game state, or a string containing an error message if restoring
-- the game fails.
restoreGame :: Config.CP -> IO (Either State String)
restoreGame config =
  E.catch (do
             f <- file config
             (x, z) <- strictDecodeCompressedFile f
             mvBkp config
             (z :: Bool) `seq` return $ Left x)
          (\ e -> case e :: IOException of
                    _ -> return (Right $
                                   "Restore failed: "
                                   ++ (unwords . lines) (show e)))

-- | Move the savegame file to a backup slot.
mvBkp :: Config.CP -> IO ()
mvBkp config =
  do
    f <- file config
    renameFile f (f ++ ".bkp")

-- | Remove the backup of the savegame. Should be called before any
-- non-error exit from the game.
rmBkp :: Config.CP -> IO ()
rmBkp config =
  do
    f <- file config
    E.catch (removeFile (f ++ ".bkp"))
      (\ e -> case e :: IOException of _ -> return ())
