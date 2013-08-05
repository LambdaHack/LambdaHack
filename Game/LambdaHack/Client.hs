{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client
  ( cmdClientAISem, cmdClientUISem
  , loopAI, loopUI, exeFrontend
  , MonadClient, MonadClientUI, MonadConnClient
  ) where

import Control.Monad
import Data.Maybe

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.AtomicSemCli
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.ClientSem
import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.LoopAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.ClientCmd
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.State
import Game.LambdaHack.Frontend
import Game.LambdaHack.Utils.Assert

storeUndo :: MonadClient m => Atomic -> m ()
storeUndo atomic = do
  maybe skip (\a -> modifyClient $ \cli -> cli {sundo = a : sundo cli})
    $ undoAtomic atomic

cmdClientAISem :: (MonadAtomic m, MonadConnClient c m)
               => CmdClientAI -> m ()
cmdClientAISem cmd = case cmd of
  CmdAtomicAI cmdA -> do
    cmds <- cmdAtomicFilterCli cmdA
    mapM_ cmdAtomicSemCli cmds
    mapM_ execCmdAtomic cmds
    mapM_ (storeUndo . CmdAtomic) cmds
  CmdQueryAI aid -> do
    cmdC <- queryAI aid
    writeConnServer cmdC

cmdClientUISem :: ( MonadAtomic m, MonadClientAbort m
                  , MonadClientUI m, MonadConnClient c m )
               => CmdClientUI -> m ()
cmdClientUISem cmd = do
  mleader <- getsClient _sleader
  case cmd of
    CmdAtomicUI cmdA -> do
      cmds <- cmdAtomicFilterCli cmdA
      mapM_ cmdAtomicSemCli cmds
      mapM_ execCmdAtomic cmds
      when (isJust mleader) $
        mapM_ (drawCmdAtomicUI False) cmds
      mapM_ (storeUndo . CmdAtomic) cmds
    SfxAtomicUI sfx -> do
      when (isJust mleader) $
        drawSfxAtomicUI False sfx
      storeUndo $ SfxAtomic sfx
    CmdQueryUI aid -> do
      assert (isJust mleader `blame` cmd) skip
      cmdH <- queryUI aid
      writeConnServer cmdH

wireSession :: (SessionUI -> State -> StateClient
                -> ConnFrontend -> ConnServer CmdClientUI -> IO ())
            -> (SessionUI -> State -> StateClient
                -> ConnFrontend -> ConnServer CmdClientAI -> IO ())
            -> Kind.COps
            -> ((FactionId -> ConnFrontend -> ConnServer CmdClientUI -> IO ())
                -> (FactionId -> ConnServer CmdClientAI -> IO ())
                -> IO ())
            -> IO ()
wireSession exeClientUI exeClientAI cops@Kind.COps{corule} exeServer = do
  -- UI config reloaded at each client start.
  sconfigUI <- mkConfigUI corule
  let !sbinding = stdBinding sconfigUI  -- evaluate to check for errors
      font = configFont sconfigUI
  defHist <- defHistory
  let cli = defStateClient defHist sconfigUI
      pos = updateCOps (const cops) emptyState
      executorAI fid =
        let noSession = assert `failure` fid
            sfconn = assert `failure` fid  -- TODO: hackish
        in exeClientAI noSession pos (cli fid True) sfconn
      executorUI fid =
        let sfconn = assert `failure` fid  -- TODO: hackish
        in exeClientUI SessionUI{..} pos (cli fid False)
  startupF font $ exeServer executorUI executorAI

-- | Wire together game content, the main loop of game clients,
-- the main game loop assigned to this frontend (possibly containing
-- the server loop, if the whole game runs in one process),
-- UI config and the definitions of game commands.
exeFrontend :: ( MonadAtomic m, MonadClientAbort m, MonadClientUI m
               , MonadConnClient CmdClientUI m
               , MonadAtomic n
               , MonadConnClient CmdClientAI n )
            => (m () -> SessionUI -> State -> StateClient
                -> ConnFrontend -> ConnServer CmdClientUI
                -> IO ())
            -> (n () -> SessionUI -> State -> StateClient
                -> ConnFrontend -> ConnServer CmdClientAI
                -> IO ())
            -> Kind.COps
            -> ((FactionId -> ConnFrontend -> ConnServer CmdClientUI -> IO ())
               -> (FactionId -> ConnServer CmdClientAI -> IO ())
               -> IO ())
            -> IO ()
exeFrontend executorUI executorAI cops exeServer = do
  let loopClientUI = loopUI cmdClientUISem
      loopClientAI = loopAI cmdClientAISem
      exeClientUI = executorUI loopClientUI
      exeClientAI = executorAI loopClientAI
  wireSession exeClientUI exeClientAI cops exeServer
