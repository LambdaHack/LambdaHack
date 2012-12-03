{-# LANGUAGE OverloadedStrings #-}
-- | Saving and restoring games and player diaries.
module Game.LambdaHack.Action.Save
  ( saveGameFile, restoreGame, rmBkpSaveDiary, saveGameBkp
  ) where

import System.Directory
import System.FilePath
import qualified Control.Exception as Ex hiding (handle)
import Control.Monad
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)  -- horrors
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Utils.File
import Game.LambdaHack.State
import Game.LambdaHack.Msg
import Game.LambdaHack.Config

-- | Save a simple serialized version of the current player diary.
saveDiary :: FilePath -> Diary -> IO ()
saveDiary configDiaryFile diary = encodeEOF configDiaryFile diary

saveLock :: MVar ()
{-# NOINLINE saveLock #-}
saveLock = unsafePerformIO newEmptyMVar

-- | Save a simple serialized version of the current state.
-- Protected by a lock to avoid corrupting the file.
saveGameFile :: ConfigUI -> State -> IO ()
saveGameFile ConfigUI{configSaveFile} state = do
  putMVar saveLock ()
  encodeEOF configSaveFile state
  takeMVar saveLock

-- | Try to create a directory. Hide errors due to,
-- e.g., insufficient permissions, because the game can run
-- in the current directory just as well.
tryCreateDir :: FilePath -> IO ()
tryCreateDir dir =
  Ex.catch
    (createDirectory dir)
    (\ e -> case e :: Ex.IOException of _ -> return ())

-- | Try to copy over data files. Hide errors due to,
-- e.g., insufficient permissions, because the game can run
-- without data files just as well.
tryCopyDataFiles :: ConfigUI -> (FilePath -> IO FilePath) -> IO ()
tryCopyDataFiles ConfigUI{ configScoresFile
                         , configRulesCfgFile
                         , configUICfgFile } pathsDataFile = do
  rulesFile  <- pathsDataFile $ takeFileName configRulesCfgFile <.> ".default"
  uiFile     <- pathsDataFile $ takeFileName configUICfgFile    <.> ".default"
  scoresFile <- pathsDataFile $ takeFileName configScoresFile
  let newRulesFile  = configRulesCfgFile <.> ".ini"
      newUIFile     = configUICfgFile    <.> ".ini"
      newScoresFile = configScoresFile
  Ex.catch
    (copyFile rulesFile newRulesFile >>
     copyFile uiFile newUIFile >>
     copyFile scoresFile newScoresFile)
    (\ e -> case e :: Ex.IOException of _ -> return ())

-- | Restore a saved game, if it exists. Initialize directory structure,
-- if needed.
restoreGame :: ConfigUI -> (FilePath -> IO FilePath) -> Text
            -> IO (Either (State, Diary, Msg) (Diary, Msg))
restoreGame config@ConfigUI{ configAppDataDir
                           , configDiaryFile
                           , configSaveFile
                           , configBkpFile } pathsDataFile title = do
  ab <- doesDirectoryExist configAppDataDir
  -- If the directory can't be created, the current directory will be used.
  unless ab $ do
    tryCreateDir configAppDataDir
    -- Possibly copy over data files. No problem if it fails.
    tryCopyDataFiles config pathsDataFile
  -- If the diary file does not exist, create an empty diary.
  -- TODO: when diary gets corrupted, start a new one, too.
  diary <-
    do db <- doesFileExist configDiaryFile
       if db
         then strictDecodeEOF configDiaryFile
         else defaultDiary
  -- If the savefile exists but we get IO errors, we show them,
  -- back up the savefile and move it out of the way and start a new game.
  -- If the savefile was randomly corrupted or made read-only,
  -- that should solve the problem. If the problems are more serious,
  -- the other functions will most probably also throw exceptions,
  -- this time without trying to fix it up.
  sb <- doesFileExist configSaveFile
  bb <- doesFileExist configBkpFile
  Ex.catch
    (if sb
       then do
         renameFile configSaveFile configBkpFile
         state <- strictDecodeEOF configBkpFile
         let msg = "Welcome back to" <+> title <> "."
         return $ Left (state, diary, msg)
       else
         if bb
           then do
             state <- strictDecodeEOF configBkpFile
             let msg = "No savefile found. Restoring from a backup savefile."
             return $ Left (state, diary, msg)
           else return $ Right (diary, "Welcome to" <+> title <> "!"))
    (\ e -> case e :: Ex.SomeException of
              _ -> let msg = "Starting a new game, because restore failed."
                             <+> "The error message was:"
                             <+> (T.unwords . T.lines) (showT e)
                   in return $ Right (diary, msg))

-- | Save the diary and a backup of the save game file, in case of crashes.
-- This is only a backup, so no problem is the game is shut down
-- before saving finishes, so we don't wait on the mvar. However,
-- if a previous save is already in progress, we skip this save.
saveGameBkp :: ConfigUI -> State -> Diary -> IO ()
saveGameBkp ConfigUI{ configDiaryFile
                    , configSaveFile
                    , configBkpFile } state diary = do
  b <- tryPutMVar saveLock ()
  when b $
    void $ forkIO $ do
      saveDiary configDiaryFile diary  -- save diary often in case of crashes
      encodeEOF configSaveFile state
      renameFile configSaveFile configBkpFile
      takeMVar saveLock

-- | Remove the backup of the savegame and save the player diary.
-- Should be called before any non-error exit from the game.
-- Sometimes the backup file does not exist and it's OK.
-- We don't bother reporting any other removal exceptions, either,
-- because the backup file is relatively unimportant.
-- We wait on the mvar, because saving the diary at game shutdown is important.
rmBkpSaveDiary :: ConfigUI -> Diary -> IO ()
rmBkpSaveDiary ConfigUI{ configDiaryFile
                       , configBkpFile } diary = do
  putMVar saveLock ()
  saveDiary configDiaryFile diary  -- save diary often in case of crashes
  bb <- doesFileExist configBkpFile
  when bb $ removeFile configBkpFile
  takeMVar saveLock
