module Game.LambdaHack.Save where

import System.Directory
import qualified Control.Exception as E hiding (handle)

import Game.LambdaHack.Utils.File
import Game.LambdaHack.State
import qualified Game.LambdaHack.Config as Config

-- | Name of the save game.
saveFile :: Config.CP -> IO FilePath
saveFile config = Config.getFile config "files" "saveFile"

-- | Name of the persistent player diary.
diaryFile :: Config.CP -> IO FilePath
diaryFile config = Config.getFile config "files" "diaryFile"

-- | We save a simple serialized version of the current state.
saveGame :: State -> Diary -> IO ()
saveGame state diary = do
  sf <- saveFile (sconfig state)
  encodeEOF sf state
  saveDiary state diary  -- save the diary often in case of crashes

-- | We save a simple serialized version of the current player diary.
saveDiary :: State -> Diary -> IO ()
saveDiary state diary = do
  df <- diaryFile (sconfig state)
  encodeEOF df diary

-- | Restore a saved game. Returns either the current game state,
-- or a string containing an error message if restoring the game fails.
restoreGame :: Config.CP -> IO (Either (State, Diary) (String, Diary))
restoreGame config = do
  diary <-
    do df <- diaryFile config
       lb <- doesFileExist df
       if lb
         then strictDecodeEOF df
         else defaultDiary
  estate <-
    E.catch
      (do mvBkp config
          sf <- saveFile config
          state <- strictDecodeEOF (sf ++ ".bkp")
          return $ Left state)
      (\ e -> case e :: E.IOException of
          _ -> return $ Right $
                 "Restore failed: " ++ (unwords . lines) (show e))
  return $ case estate of
    Left state -> Left (state, diary)
    Right msg  -> Right (msg, diary)

-- | Move the savegame file to a backup slot.
mvBkp :: Config.CP -> IO ()
mvBkp config = do
  f <- saveFile config
  renameFile f (f ++ ".bkp")

-- | Remove the backup of the savegame and save the player diary.
-- Should be called before any non-error exit from the game.
-- Sometimes the backup file does not exist and it's OK.
-- We don't bother reporting any other removal exceptions, either,
-- because the backup file is relatively unimportant.
rmBkpSaveDiary :: State -> Diary -> IO ()
rmBkpSaveDiary state diary = do
  saveDiary state diary  -- save the diary often in case of crashes
  sf <- saveFile (sconfig state)
  E.catch (removeFile (sf ++ ".bkp"))
    (\ e -> case e :: E.IOException of _ -> return ())
