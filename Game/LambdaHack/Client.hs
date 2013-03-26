{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
-- See https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture.
module Game.LambdaHack.Client
  ( cmdClientAISem, cmdClientUISem
  , loopAI, loopUI, exeFrontend
  , MonadClient, MonadClientUI, MonadClientConn
  ) where

import Control.Concurrent
import Control.Monad
import Data.Maybe

import Game.LambdaHack.Action
import Game.LambdaHack.AtomicCmd
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.AtomicSemCli
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.ClientSem
import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.LoopAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.ClientCmd
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert

storeUndo :: MonadClient m => Atomic -> m ()
storeUndo atomic = do
  maybe skip (\a -> modifyClient $ \cli -> cli {sundo = a : sundo cli})
    $ undoAtomic atomic

cmdClientAISem :: (MonadAtomic m, MonadClientConn c m)
               => CmdClientAI -> m ()
cmdClientAISem cmd = case cmd of
  CmdAtomicAI cmdA -> do
    cmds <- cmdAtomicFilterCli cmdA
    mapM_ cmdAtomicSemCli cmds
    mapM_ execCmdAtomic cmds
    mapM_ (storeUndo . CmdAtomic) cmds
  CmdQueryAI aid -> do
    cmdC <- queryAI aid
    writeConnFromClient cmdC

cmdClientUISem :: ( MonadAtomic m, MonadClientAbort m
                  , MonadClientUI m, MonadClientConn c m )
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
      writeConnFromClient cmdH

wireSession :: (SessionUI -> State -> StateClient -> Conn CmdClientUI -> IO ())
            -> (SessionUI -> State -> StateClient -> Conn CmdClientAI -> IO ())
            -> Kind.COps
            -> ((FactionId -> Conn CmdClientUI -> IO ())
                -> (FactionId -> Conn CmdClientAI -> IO ())
                -> IO ())
            -> IO ()
wireSession exeClientUI exeClientAI cops@Kind.COps{corule} exeServer = do
  -- UI config reloaded at each client start.
  sconfigUI <- mkConfigUI corule
  smvarUI <- newEmptyMVar
  let !sbinding = stdBinding sconfigUI  -- evaluate to check for errors
      font = configFont sconfigUI
  defHist <- defHistory
  let cli = defStateClient defHist sconfigUI
      loc = updateCOps (const cops) emptyState
      executorAI _sfs fid =
        let noSession = assert `failure` fid
        in exeClientAI noSession loc (cli fid True)
      executorUI sfs fid =
        exeClientUI SessionUI{..} loc (cli fid False)
  startup font $ \sfs -> exeServer (executorUI sfs) (executorAI sfs)

-- | Wire together game content, the main loop of game clients,
-- the main game loop assigned to this frontend (possibly containing
-- the server loop, if the whole game runs in one process),
-- UI config and the definitions of game commands.
exeFrontend :: ( MonadAtomic m, MonadClientAbort m, MonadClientUI m
               , MonadClientConn CmdClientUI m
               , MonadAtomic n
               , MonadClientConn CmdClientAI n )
            => (m () -> SessionUI -> State -> StateClient -> Conn CmdClientUI
                -> IO ())
            -> (n () -> SessionUI -> State -> StateClient -> Conn CmdClientAI
                -> IO ())
            -> Kind.COps
            -> ((FactionId -> Conn CmdClientUI -> IO ())
               -> (FactionId -> Conn CmdClientAI -> IO ())
               -> IO ())
            -> IO ()
exeFrontend executorUI executorAI cops exeServer = do
  let loopClientUI = loopUI cmdClientUISem
      loopClientAI = loopAI cmdClientAISem
      exeClientUI = executorUI loopClientUI
      exeClientAI = executorAI loopClientAI
  wireSession exeClientUI exeClientAI cops exeServer
