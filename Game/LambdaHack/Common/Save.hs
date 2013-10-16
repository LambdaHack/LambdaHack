-- | Saving and restoring server game state.
module Game.LambdaHack.Common.Save
  ( ChanSave, saveToChan, wrapInSaves
  ) where

import Control.Concurrent
import qualified Control.Exception as Ex hiding (handle)
import Control.Monad
import Data.Binary

import Game.LambdaHack.Utils.File
import Game.LambdaHack.Utils.Thread

type ChanSave a = MVar (Maybe a)

saveToChan :: ChanSave a -> a -> IO ()
saveToChan toSave s = do
  -- Wipe out previous candidates for saving.
  void $ tryTakeMVar toSave
  putMVar toSave $ Just s

-- TODO: to have crash saves, send state to server save channel each turn
-- and have another mvar, asking for a save with the last state;
-- this mvar is permanently true on clients, but only set on server
-- in finally and each time bkp save is requested; finally should also
-- send save request to all clients (using the last state from the save
-- channel for client connection data, etc.)
-- All this is not needed if we bkp save each turn, but that's costly.

-- | Repeatedly save a simple serialized version of the current state.
loopSave :: Binary a => (a -> FilePath) -> ChanSave a -> IO ()
loopSave saveFile toSave =
  loop
 where
  loop = do
    -- Wait until anyting to save.
    ms <- takeMVar toSave
    case ms of
      Just s -> do
        encodeEOF (saveFile s) s
        -- Wait until the save finished. During that time, the mvar
        -- is continually updated to newest state values.
        loop
      Nothing -> return ()  -- exit

wrapInSaves :: Binary a => (a -> FilePath) -> (ChanSave a -> IO ()) -> IO ()
wrapInSaves saveFile exe = do
  -- We don't merge this with the other calls to waitForChildren,
  -- because, e.g., for server, we don't want to wait for clients to exit,
  -- if the server crashes (but we wait for the save to finish).
  children <- newMVar []
  toSave <- newEmptyMVar
  void $ forkChild children $ loopSave saveFile toSave
  let fin = do
        -- Wait until the last save starts and tell the save thread to end.
        putMVar toSave Nothing
        -- Wait until the save thread ends.
        waitForChildren children
  exe toSave `Ex.finally` fin
  -- The creation of, e.g., the initial client state, is outside the 'finally'
  -- clause, but this is OK, since no saves are ordered until 'runActionCli'.
  -- We save often, not only in the 'finally' section, in case of
  -- power outages, kill -9, GHC runtime crashes, etc. For internal game
  -- crashes, C-c, etc., the finalizer would be enough.
  -- If we implement incremental saves, saving often will help
  -- to spread the cost, to avoid a long pause at game exit.
