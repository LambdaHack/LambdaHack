{-# LANGUAGE OverloadedStrings #-}
-- | Saving and restoring server game state.
module Game.LambdaHack.Server.Action.Save
  ( saveGameBkpSer, saveGameSer, restoreGameSer
  ) where

import Control.Concurrent
import qualified Control.Exception as Ex hiding (handle)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Msg
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import Game.LambdaHack.Utils.File

saveLock :: MVar ()
{-# NOINLINE saveLock #-}
saveLock = unsafePerformIO newEmptyMVar

-- TODO: make sure it's executed eagerly.
-- | Save game to the backup savefile, in case of crashes.
-- This is only a backup, so no problem is the game is shut down
-- before saving finishes, so we don't wait on the mvar. However,
-- if a previous save is already in progress, we skip this save.
saveGameBkpSer :: Config -> State -> StateServer -> IO ()
saveGameBkpSer Config{configAppDataDir} s ser = do
  b <- tryPutMVar saveLock ()
  when b $
    void $ forkIO $ do
      let saveFile = configAppDataDir </> "server.sav"
          saveFileBkp = saveFile <.> ".bkp"
      encodeEOF saveFile (s, ser)
      renameFile saveFile saveFileBkp
      takeMVar saveLock

-- | Save a simple serialized version of the current state.
-- Protected by a lock to avoid corrupting the file.
saveGameSer :: Config -> State -> StateServer -> IO ()
saveGameSer Config{configAppDataDir} s ser = do
  putMVar saveLock ()
  let saveFile = configAppDataDir </> "server.sav"
  encodeEOF saveFile (s, ser)
  takeMVar saveLock

-- | Restore a saved game, if it exists. Initialize directory structure
-- and cope over data files, if needed.
restoreGameSer :: Config -> (FilePath -> IO FilePath) -> Text
               -> IO (Either (State, StateServer, Msg) Msg)
restoreGameSer Config{ configAppDataDir
                     , configRulesCfgFile
                     , configScoresFile }
               pathsDataFile title = do
  -- Create user data directory and copy files, if not already there.
  tryCreateDir configAppDataDir
  tryCopyDataFiles pathsDataFile
    [ (configRulesCfgFile <.> ".default", configRulesCfgFile <.> ".ini")
    , (configScoresFile, configScoresFile) ]
  let saveFile = configAppDataDir </> "server.sav"
      saveFileBkp = saveFile <.> ".bkp"
  sb <- doesFileExist saveFile
  bb <- doesFileExist saveFileBkp
  when sb $ renameFile saveFile saveFileBkp
  -- If the savefile exists but we get IO or decoding errors, we show them,
  -- back up the savefile, move it out of the way and start a new game.
  -- If the savefile was randomly corrupted or made read-only,
  -- that should solve the problem. Serious IO problems (e.g. failure
  -- to create a user data directory) terminate the program with an exception.
  Ex.catch
    (if sb
       then do
         (s, ser) <- strictDecodeEOF saveFileBkp
         let msg = "Welcome back to" <+> title <> "."
         return $ Left (s, ser, msg)
       else
         if bb
           then do
             (s, ser) <- strictDecodeEOF saveFileBkp
             let msg = "No savefile found. Restoring from a backup savefile."
             return $ Left (s, ser {squit = Just True}, msg)
           else do
             let msg = "Welcome to" <+> title <> "!"
             return $ Right msg
    )
    (\ e -> case e :: Ex.SomeException of
              _ -> let msg = "Starting a new game, because restore failed."
                             <+> "The error message was:"
                             <+> (T.unwords . T.lines) (showT e)
                   in return $ Right msg
    )
