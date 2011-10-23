module Save where

import System.Directory
import qualified Control.Exception as E hiding (handle)

import Utils.File
import State
import qualified Config

-- | Name of the save game.
file :: Config.CP -> IO FilePath
file config = Config.getFile config "files" "saveGame"

-- | We save a simple serialized version of the current state
-- and random generator.
saveGame :: (State, String) -> IO ()
saveGame (state, g) = do
  f <- file (sconfig state)
  encodeEOF f (state, g)

-- | Restore a saved game. Returns either the current game state,
-- or a string containing an error message if restoring the game fails.
restoreGame :: Config.CP -> IO (Either (State, String) String)
restoreGame config =
  E.catch (do
             mvBkp config
             f <- file config
             sg <- strictDecodeEOF (f ++ ".bkp")
             return (Left sg))
          (\ e -> case e :: E.IOException of
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
-- non-error exit from the game. Sometimes it does not exist and it's OK.
-- We don't bother reporting any other exceptions, either, because the file
-- is relatively unimportant and because most probably the exception
-- would be reported for the main savefile, where it should not be overlooked.
rmBkp :: Config.CP -> IO ()
rmBkp config =
  do
    f <- file config
    E.catch (removeFile (f ++ ".bkp"))
      (\ e -> case e :: E.IOException of _ -> return ())
