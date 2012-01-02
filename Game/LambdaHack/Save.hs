module Game.LambdaHack.Save where

import System.Directory
import qualified Control.Exception as E hiding (handle)

import Game.LambdaHack.Utils.File
import Game.LambdaHack.State
import qualified Game.LambdaHack.Config as Config

-- | Name of the save game.
saveFile :: Config.CP -> IO FilePath
saveFile config = Config.getFile config "files" "saveFile"

-- | Name of the persistent player data.
playerFile :: Config.CP -> IO FilePath
playerFile config = Config.getFile config "files" "playerFile"

-- | We save a simple serialized version of the current state.
saveGame :: State -> IO ()
saveGame state = do
  sf <- saveFile (sconfig state)
  encodeEOF sf state
  pf <- playerFile (sconfig state)
  encodeEOF pf state

-- | Restore a saved game. Returns either the current game state,
-- or a string containing an error message if restoring the game fails.
restoreGame :: Config.CP -> IO (Either State String)
restoreGame config =
  E.catch (do mvBkp config
              sf <- saveFile config
              state <- strictDecodeEOF (sf ++ ".bkp")
              pf <- playerFile config
              state2 <- strictDecodeEOF pf
              let _ = state2 :: State -- TODO
              return (Left state))
          (\ e -> case e :: E.IOException of
                    _ -> return (Right $ "Restore failed: "
                                         ++ (unwords . lines) (show e)))

-- | Move the savegame file to a backup slot.
mvBkp :: Config.CP -> IO ()
mvBkp config = do
  f <- saveFile config
  renameFile f (f ++ ".bkp")

-- | Remove the backup of the savegame. Should be called before any
-- non-error exit from the game. Sometimes it does not exist and it's OK.
-- We don't bother reporting any other exceptions, either, because the file
-- is relatively unimportant and because most probably the exception
-- would be reported for the main savefile, where it should not be overlooked.
rmBkp :: Config.CP -> IO ()
rmBkp config = do
  f <- saveFile config
  E.catch (removeFile (f ++ ".bkp"))
    (\ e -> case e :: E.IOException of _ -> return ())
