module Game.LambdaHack.Save
  ( saveFile, saveGame, restoreGame, rmBkpSaveDiary, saveGameBkp
  ) where

import System.Directory
import System.FilePath
import qualified Control.Exception as E hiding (handle)
import Control.Monad

-- Cabal
import qualified Paths_LambdaHack as Self (getDataFileName)

import Game.LambdaHack.Utils.File
import Game.LambdaHack.State
import qualified Game.LambdaHack.Config as Config

-- | Name of the save game.
saveFile :: Config.CP -> IO FilePath
saveFile config = Config.getFile config "files" "saveFile"

-- | Name of the backup of the save game.
bkpFile :: Config.CP -> IO FilePath
bkpFile config = do
  sfile <- saveFile config
  return $ (sfile ++ ".bkp")

-- | Name of the persistent player diary.
diaryFile :: Config.CP -> IO FilePath
diaryFile config = Config.getFile config "files" "diaryFile"

-- | We save a simple serialized version of the current player diary.
saveDiary :: State -> Diary -> IO ()
saveDiary state diary = do
  dfile <- diaryFile (sconfig state)
  encodeEOF dfile diary

-- | We save a simple serialized version of the current state.
saveGame :: State -> Diary -> IO ()
saveGame state diary = do
  sfile <- saveFile (sconfig state)
  encodeEOF sfile state
  saveDiary state diary  -- save the diary often in case of crashes

-- | Try to create a directory. Hide errors due to,
-- e.g., insufficient permissions, because the game can run
-- in the current directory just as well.
tryCreateDir :: FilePath -> IO ()
tryCreateDir dir =
  E.catch
    (createDirectory dir)
    (\ e -> case e :: E.IOException of _ -> return ())

-- | Try to copy over data files. Hide errors due to,
-- e.g., insufficient permissions, because the game can run
-- without data files just as well.
tryCopyDataFiles :: FilePath -> IO ()
tryCopyDataFiles dirNew = do
  configFile <- Self.getDataFileName "config.default"
  scoresFile <- Self.getDataFileName "scores"
  let configNew = combine dirNew "config"
      scoresNew = combine dirNew "scores"
--  E.catch
  (do copyFile configFile configNew
      copyFile scoresFile scoresNew)
--    (\ e -> case e :: E.IOException of _ -> return ())

-- | Restore a saved game, if it exists. Initialize directory structure,
-- if needed.
restoreGame :: Config.CP -> String -> IO (Either (State, Diary) (String, Diary))
restoreGame config title = do
  appData <- Config.appDataDir
  ab <- doesDirectoryExist appData
  -- If the directory can't be created, the current directory will be used.
  unless ab $ tryCreateDir appData
  -- Possibly copy over data files. No problem if it fails.
  tryCopyDataFiles appData
  -- If the diary file does not exist, create an empty diary.
  diary <-
    do dfile <- diaryFile config
       db <- doesFileExist dfile
       if db
         then strictDecodeEOF dfile
         else defaultDiary
  -- If the savefile exists but we get IO errors, we show them,
  -- back up the savefile and move it out of the way and start a new game.
  -- If the savefile was randomly corrupted or made read-only,
  -- that should solve the problem. If the problems are more serious,
  -- the other functions will most probably also throw exceptions,
  -- this time without trying to fix it up.
  sfile <- saveFile config
  sb <- doesFileExist sfile
  if sb
    then E.catch
      (do mvBkp config
          bfile <- bkpFile config
          state <- strictDecodeEOF bfile
          return $ Left (state, diary))
      (\ e -> case e :: E.SomeException of
                _ -> let msg = "Starting a new game, because restore failed. The error message was: "
                               ++ (unwords . lines) (show e)
                     in return $ Right (msg, diary))
    else
      return $ Right ("Welcome to " ++ title ++ "!", diary)

-- | Move the savegame file to a backup slot.
mvBkp :: Config.CP -> IO ()
mvBkp config = do
  sfile <- saveFile config
  bfile <- bkpFile config
  renameFile sfile bfile

-- | We save a backup of the save game file, in case of crashes.
saveGameBkp :: State -> Diary -> IO ()
saveGameBkp state diary = do
  saveGame state diary
  mvBkp (sconfig state)

-- | Remove the backup of the savegame and save the player diary.
-- Should be called before any non-error exit from the game.
-- Sometimes the backup file does not exist and it's OK.
-- We don't bother reporting any other removal exceptions, either,
-- because the backup file is relatively unimportant.
rmBkpSaveDiary :: State -> Diary -> IO ()
rmBkpSaveDiary state diary = do
  saveDiary state diary  -- save the diary often in case of crashes
  bfile <- bkpFile (sconfig state)
  bb <- doesFileExist bfile
  when bb $ removeFile bfile
