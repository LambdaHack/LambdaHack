{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client.HandleResponseM
  ( handleResponseAI, handleSelfAI, handleResponseUI, handleSelfUI
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.AI
import Game.LambdaHack.Client.HandleAtomicM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.ProtocolM
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response

storeUndo :: MonadClient m => CmdAtomic -> m ()
storeUndo _atomic =
  maybe (return ()) (\a -> modifyClient $ \cli -> cli {sundo = a : sundo cli})
    Nothing   -- TODO: undoCmdAtomic atomic

handleResponseAI :: ( MonadClientSetup m
                    , MonadAtomic m
                    , MonadClientWriteRequest RequestAI m )
                 => ResponseAI -> m ()
handleResponseAI cmd = case cmd of
  RespUpdAtomicAI cmdA -> do
    handleSelfAI cmdA
    sendRequest (ReqAINop, Nothing)
  RespQueryAI -> do
    cmdC <- queryAI
    sendRequest cmdC
  RespNonLeaderQueryAI aid -> do
    cmdC <- nonLeaderQueryAI aid
    sendRequest cmdC

handleSelfAI :: ( MonadClientSetup m
                , MonadAtomic m
                , MonadClientWriteRequest RequestAI m )
             => UpdAtomic -> m ()
handleSelfAI cmdA = do
  cmds <- cmdAtomicFilterCli cmdA
  mapM_ (\c -> cmdAtomicSemCli c
               >> execUpdAtomic c) cmds
  mapM_ (storeUndo . UpdAtomic) cmds

handleResponseUI :: ( MonadClientSetup m
                    , MonadClientUI m
                    , MonadAtomic m
                    , MonadClientWriteRequest RequestUI m )
                 => ResponseUI -> m ()
handleResponseUI cmd = case cmd of
  RespUpdAtomicUI cmdA -> do
    handleSelfUI cmdA
    sendRequest (ReqUINop, Nothing)
  RespSfxAtomicUI sfx -> do
    displayRespSfxAtomicUI False sfx
    storeUndo $ SfxAtomic sfx
    sendRequest (ReqUINop, Nothing)
  RespQueryUI -> do
    cmdH <- queryUI
    sendRequest cmdH

handleSelfUI :: ( MonadClientSetup m
                , MonadClientUI m
                , MonadAtomic m
                , MonadClientWriteRequest RequestUI m )
             => UpdAtomic -> m ()
handleSelfUI cmdA = do
  cmds <- cmdAtomicFilterCli cmdA
  let handle c = do
        !oldDiscoKind <- getsClient sdiscoKind
        !oldDiscoAspect <- getsClient sdiscoAspect
        cmdAtomicSemCli c
        execUpdAtomic c
        displayRespUpdAtomicUI False oldDiscoKind oldDiscoAspect c
  mapM_ handle cmds
  mapM_ (storeUndo . UpdAtomic) cmds  -- TODO: only store cmdA?
