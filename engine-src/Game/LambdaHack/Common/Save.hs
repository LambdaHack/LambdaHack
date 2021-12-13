-- | Saving and restoring game state, used by both server and clients.
module Game.LambdaHack.Common.Save
  ( ChanSave, saveToChan, wrapInSaves, restoreGame
  , compatibleVersion, delayPrint
  , saveNameCli, saveNameSer, bkpAllSaves
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , loopSave
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Control.Exception as Ex
import           Data.Binary
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Version
import           System.FilePath
import           System.IO (hFlush, stdout)
import qualified System.Random.SplitMix32 as SM

import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.File
import Game.LambdaHack.Common.Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Types
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Core.Random

type ChanSave a = MVar (Maybe a)

saveToChan :: ChanSave a -> a -> IO ()
saveToChan toSave s = do
  -- Wipe out previous candidates for saving.
  void $ tryTakeMVar toSave
  putMVar toSave $ Just s

-- | Repeatedly save serialized snapshots of current state.
--
-- Running with @-N2@ ca reduce @Max pause@ from 0.2s to 0.01s
-- and @bytes copied during GC@ 10-fold, but framerate nor the frequency
-- of not making a backup save are unaffected (at standard backup settings),
-- even with null frontend, because saving takes so few resources.
-- So, generally, backup save settings are relevant only due to latency
-- impact on very slow computers or in JS.
loopSave :: Binary a => COps -> (a -> FilePath) -> ChanSave a -> IO ()
loopSave cops stateToFileName toSave =
  loop
 where
  loop = do
    -- Wait until anyting to save.
    ms <- takeMVar toSave
    case ms of
      Just s -> do
        dataDir <- appDataDir
        tryCreateDir (dataDir </> "saves")
        let fileName = stateToFileName s
        yield  -- minimize UI lag due to saving
        encodeEOF (dataDir </> "saves" </> fileName)
                  (rexeVersion $ corule cops)
                  s
        -- Wait until the save finished. During that time, the mvar
        -- is continually updated to newest state values.
        loop
      Nothing -> return ()  -- exit

wrapInSaves :: Binary a
            => COps -> (a -> FilePath) -> (ChanSave a -> IO ()) -> IO ()
{-# INLINE wrapInSaves #-}
wrapInSaves cops stateToFileName exe = do
  -- We don't merge this with the other calls to waitForChildren,
  -- because, e.g., for server, we don't want to wait for clients to exit,
  -- if the server crashes (but we wait for the save to finish).
  toSave <- newEmptyMVar
  a <- async $ loopSave cops stateToFileName toSave
  link a
  let fin = do
        -- Wait until the last save (if any) starts
        -- and tell the save thread to end.
        putMVar toSave Nothing
        -- Wait 0.5s to flush debug and then until the save thread ends.
        threadDelay 500000
        wait a
  exe toSave `Ex.finally` fin
  -- The creation of, e.g., the initial client state, is outside the 'finally'
  -- clause, but this is OK, since no saves are ordered until 'runActionCli'.
  -- We save often, not only in the 'finally' section, in case of
  -- power outages, kill -9, GHC runtime crashes, etc. For internal game
  -- crashes, C-c, etc., the finalizer would be enough.
  -- If we implement incremental saves, saving often will help
  -- to spread the cost, to avoid a long pause at game exit.

-- | Restore a saved game, if it exists. Initialize directory structure
-- and copy over data files, if needed.
restoreGame :: Binary a
            => RuleContent -> ClientOptions -> FilePath -> IO (Maybe a)
restoreGame corule clientOptions fileName = do
  -- Create user data directory and copy files, if not already there.
  dataDir <- appDataDir
  tryCreateDir dataDir
  let path = dataDir </> "saves" </> fileName
  saveExists <- doesFileExist path
  -- If the savefile exists but we get IO or decoding errors,
  -- we show them and start a new game. If the savefile was randomly
  -- corrupted or made read-only, that should solve the problem.
  -- OTOH, serious IO problems (e.g. failure to create a user data directory)
  -- terminate the program with an exception.
  res <- Ex.try $
    if saveExists then do
      let vExe1 = rexeVersion corule
      (vExe2, s) <- strictDecodeEOF path
      if compatibleVersion vExe1 vExe2
      then return $! s `seq` Just s
      else do
        let msg = "Savefile" <+> T.pack path
                  <+> "from an incompatible version"
                  <+> T.pack (showVersion vExe2)
                  <+> "detected while trying to restore"
                  <+> T.pack (showVersion vExe1)
                  <+> "game."
        fail $ T.unpack msg
    else return Nothing
  let handler :: Ex.SomeException -> IO (Maybe a)
      handler e = do
        moveAside <- bkpAllSaves corule clientOptions
        let msg = "Restore failed."
                  <+> (if moveAside
                      then "The wrong file has been moved aside."
                      else "")
                  <+> "The error message is:"
                  <+> (T.unwords . T.lines) (tshow e)
        delayPrint msg
        return Nothing
  either handler return res

-- Minor version discrepancy permitted.
compatibleVersion :: Version -> Version -> Bool
compatibleVersion v1 v2 = take 3 (versionBranch v1) == take 3 (versionBranch v2)

delayPrint :: Text -> IO ()
delayPrint t = do
  smgen <- SM.newSMGen
  let (delay, _) = nextRandom 10000 smgen
  threadDelay $ 100 * delay  -- try not to interleave saves with other clients
  T.hPutStr stdout $! t <> "\n"  -- hPutStrLn not atomic enough
  hFlush stdout

saveNameCli :: RuleContent -> FactionId -> String
saveNameCli corule side =
  let gameShortName =
        case words $ rtitle corule of
          w : _ -> w
          _ -> "Game"
  in gameShortName
     ++ ".team_" ++ show (fromEnum side)
     ++ ".sav"

saveNameSer :: RuleContent -> String
saveNameSer corule =
  let gameShortName =
        case words $ rtitle corule of
          w : _ -> w
          _ -> "Game"
  in gameShortName ++ ".server.sav"

bkpAllSaves :: RuleContent -> ClientOptions -> IO Bool
bkpAllSaves corule clientOptions = do
  dataDir <- appDataDir
  let benchmark = sbenchmark clientOptions
      defPrefix = ssavePrefixCli defClientOptions
      moveAside = not benchmark && ssavePrefixCli clientOptions == defPrefix
      bkpOneSave name = do
        let pathSave bkp = dataDir </> "saves" </> bkp <> defPrefix <> name
        b <- doesFileExist (pathSave "")
        when b $ renameFile (pathSave "") (pathSave "bkp.")
      bkpAll = do
        bkpOneSave $ saveNameSer corule
        forM_ [-199..199] $ \n ->
          bkpOneSave $ saveNameCli corule (toEnum n)
  when moveAside bkpAll
  return moveAside
