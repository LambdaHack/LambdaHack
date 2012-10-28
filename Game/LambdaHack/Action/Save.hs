-- | Saving and restoring games and player diaries.
module Game.LambdaHack.Action.Save
  ( saveGameFile, restoreGame, rmBkpSaveDiary, saveGameBkp
  ) where

import System.Directory
import System.FilePath
import qualified Control.Exception as E hiding (handle)
import Control.Monad
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)  -- horrors

import Game.LambdaHack.Utils.File
import Game.LambdaHack.State
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Action.ConfigIO as ConfigIO
import Game.LambdaHack.Msg

-- | Name of the save game.
saveFile :: Config.CP -> IO FilePath
saveFile config = ConfigIO.getFile config "files" "saveFile"

-- | Name of the backup of the save game.
bkpFile :: Config.CP -> IO FilePath
bkpFile config = do
  sfile <- saveFile config
  return $ sfile ++ ".bkp"

-- | Name of the persistent player diary.
diaryFile :: Config.CP -> IO FilePath
diaryFile config = ConfigIO.getFile config "files" "diaryFile"

-- | Save a simple serialized version of the current player diary.
saveDiary :: Config.CP -> Diary -> IO ()
saveDiary config diary = do
  dfile <- diaryFile config
  encodeEOF dfile diary

saveLock :: MVar ()
{-# NOINLINE saveLock #-}
saveLock = unsafePerformIO newEmptyMVar

-- TODO: add similar locks to all saved files and save all files
-- in the background, just as savefile sometimes is.
-- | Save a simple serialized version of the current state.
-- Protected by a lock to avoid corrupting the file.
saveGameFile :: State -> IO ()
saveGameFile state = do
  putMVar saveLock ()
  sfile <- saveFile (sconfig state)
  encodeEOF sfile state
  takeMVar saveLock

-- | Try to create a directory. Hide errors due to,
-- e.g., insufficient permissions, because the game can run
-- in the current directory just as well.
tryCreateDir :: FilePath -> IO ()
tryCreateDir dir =
  E.catch
    (createDirectory dir)
    (\ e -> case e :: E.IOException of _ -> return ())

-- TODO: perhaps take the target "scores" file name from config.
-- TODO: perhaps source and "config", too, to be able to change all
-- in one place.
-- | Try to copy over data files. Hide errors due to,
-- e.g., insufficient permissions, because the game can run
-- without data files just as well.
tryCopyDataFiles :: (FilePath -> IO FilePath) -> FilePath -> IO ()
tryCopyDataFiles pathsDataFile dirNew = do
  configFile <- pathsDataFile "config.default"
  scoresFile <- pathsDataFile "scores"
  let configNew = combine dirNew "config"
      scoresNew = combine dirNew "scores"
  E.catch
    (copyFile configFile configNew >>
     copyFile scoresFile scoresNew)
    (\ e -> case e :: E.IOException of _ -> return ())

-- | Restore a saved game, if it exists. Initialize directory structure,
-- if needed.
restoreGame :: (FilePath -> IO FilePath) -> Config.CP -> String
            -> IO (Either (State, Diary, Msg) (Diary, Msg))
restoreGame pathsDataFile config title = do
  appData <- ConfigIO.appDataDir
  ab <- doesDirectoryExist appData
  -- If the directory can't be created, the current directory will be used.
  unless ab $ do
    tryCreateDir appData
    -- Possibly copy over data files. No problem if it fails.
    tryCopyDataFiles pathsDataFile appData
  -- If the diary file does not exist, create an empty diary.
  -- TODO: when diary gets corrupted, start a new one, too.
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
  bfile <- bkpFile config
  sb <- doesFileExist sfile
  bb <- doesFileExist bfile
  E.catch
    (if sb
       then do
         mvBkp config
         state <- strictDecodeEOF bfile
         let msg = "Welcome back to " ++ title ++ "."
         return $ Left (state, diary, msg)
       else
         if bb
           then do
             state <- strictDecodeEOF bfile
             let msg = "No savefile found. Restoring from a backup savefile."
             return $ Left (state, diary, msg)
           else return $ Right (diary, "Welcome to " ++ title ++ "!"))
    (\ e -> case e :: E.SomeException of
              _ -> let msg = "Starting a new game, because restore failed. "
                             ++ "The error message was: "
                             ++ (unwords . lines) (show e)
                   in return $ Right (diary, msg))

-- | Move the savegame file to a backup slot.
mvBkp :: Config.CP -> IO ()
mvBkp config = do
  sfile <- saveFile config
  bfile <- bkpFile config
  renameFile sfile bfile

-- | Save the diary and a backup of the save game file, in case of crashes.
-- This is only a backup, so no problem is the game is shut down
-- before saving finishes, so we don't wait on the mvar. However,
-- if a previous save is already in progress, we skip this save.
saveGameBkp :: State -> Diary -> IO ()
saveGameBkp state diary = do
  b <- tryPutMVar saveLock ()
  let config = sconfig state
  when b $
    void $ forkIO $ do
      saveDiary config diary  -- save the diary often in case of crashes
      sfile <- saveFile config
      encodeEOF sfile state
      mvBkp (sconfig state)
      takeMVar saveLock

-- | Remove the backup of the savegame and save the player diary.
-- Should be called before any non-error exit from the game.
-- Sometimes the backup file does not exist and it's OK.
-- We don't bother reporting any other removal exceptions, either,
-- because the backup file is relatively unimportant.
-- We wait on the mvar, because saving the diary at game shutdown is important.
rmBkpSaveDiary :: Config.CP -> Diary -> IO ()
rmBkpSaveDiary config diary = do
  putMVar saveLock ()
  saveDiary config diary  -- save the diary often in case of crashes
  bfile <- bkpFile config
  bb <- doesFileExist bfile
  when bb $ removeFile bfile
  takeMVar saveLock
