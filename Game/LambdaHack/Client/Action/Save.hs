{-# LANGUAGE OverloadedStrings #-}
-- | Saving and restoring client game state.
module Game.LambdaHack.Client.Action.Save
  ( saveGameCli, restoreGameCli
  ) where

import Control.Concurrent
import qualified Control.Exception as Ex hiding (handle)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.State
import Game.LambdaHack.Utils.File

-- TODO: Refactor the client and server Save.hs, after
-- https://github.com/kosmikus/LambdaHack/issues/37.

_saveLock :: MVar ()
{-# NOINLINE _saveLock #-}
_saveLock = unsafePerformIO newEmptyMVar

-- | Save game to the backup savefile, in case of crashes.
-- This is only a backup, so no problem is the game is shut down
-- before saving finishes, so we don't wait on the mvar. However,
-- if a previous save is already in progress, we skip this save.
saveGameBkpCli :: String -> ConfigUI -> State -> StateClient -> IO ()
saveGameBkpCli saveName ConfigUI{configAppDataDirUI} s cli = do
--  b <- tryPutMVar saveLock ()
--  when b $
--    void $ forkIO $ do
      let saveFile = configAppDataDirUI </> saveName
          saveFileBkp = saveFile <.> ".bkp"
      encodeEOF saveFile (s, cli)
      renameFile saveFile saveFileBkp
--      takeMVar saveLock

-- | Save a simple serialized version of the current state.
-- Protected by a lock to avoid corrupting the file.
saveGameCli :: String -> Bool -> ConfigUI -> State -> StateClient -> IO ()
saveGameCli saveName True configUI s cl =
  saveGameBkpCli saveName configUI s cl
saveGameCli saveName False ConfigUI{configAppDataDirUI} s cli = do
--  putMVar saveLock ()
  let saveFile = configAppDataDirUI </> saveName
  encodeEOF saveFile (s, cli)
--  takeMVar saveLock

-- | Restore a saved game, if it exists. Initialize directory structure,
-- if needed.
restoreGameCli :: String -> ConfigUI -> (FilePath -> IO FilePath) -> Text
               -> IO (Either (State, StateClient, Msg) Msg)
restoreGameCli saveName ConfigUI{ configAppDataDirUI
                                , configUICfgFile }
               pathsDataFile title = do
  tryCreateDir configAppDataDirUI
  tryCopyDataFiles pathsDataFile
    [(configUICfgFile <.> ".default", configUICfgFile <.> ".ini")]
  let saveFile = configAppDataDirUI </> saveName
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
       (s, cli) <- strictDecodeEOF saveFileBkp
       let msg = "Welcome back to" <+> title <> "."
       return $ Left (s, cli, msg)
     else
       if bb
       then do
         (s, cli) <- strictDecodeEOF saveFileBkp
         let msg =
               "No client savefile found. Restoring from a backup savefile."
         return $ Left (s, cli, msg)
       else do
         let msg = "Welcome to" <+> title <> "!"
         return $ Right msg
    )
    (\ e -> case e :: Ex.SomeException of
              _ -> let msg =
                         "Starting a new game, because client restore failed."
                         <+> "The error message was:"
                         <+> (T.unwords . T.lines) (showT e)
                   in return $ Right msg
    )
