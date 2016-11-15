{-# LANGUAGE FlexibleContexts, FunctionalDependencies, RankNTypes, TupleSections
             #-}
-- | The client-server communication monads.
module Game.LambdaHack.Client.ProtocolM
  ( MonadClientReadResponse(..), MonadClientWriteRequest(..)
  , initAI, initUI, handleSelfAI, handleSelfUI
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.HandleAtomicM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Vector

class MonadClient m => MonadClientReadResponse resp m | m -> resp where
  receiveResponse  :: m resp

class MonadClient m => MonadClientWriteRequest req m | m -> req where
  sendRequest  :: req -> m ()

initAI :: MonadClient m => DebugModeCli -> m ()
{-# INLINABLE initAI #-}
initAI sdebugCli = do
  modifyClient $ \cli -> cli {sdebugCli}
  side <- getsClient sside
  debugPossiblyPrint $ "AI client" <+> tshow side <+> "initializing."

initUI :: MonadClientUI m => KeyKind -> Config -> DebugModeCli -> m ()
{-# INLINABLE initUI #-}
initUI copsClient sconfig sdebugCli = do
  modifyClient $ \cli ->
    cli { sxhair = TVector $ Vector 1 1  -- a step south-east, less alarming
        , sdebugCli }
  side <- getsClient sside
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "initializing."
  -- Start the frontend.
  schanF <- chanFrontend sdebugCli
  let !sbinding = stdBinding copsClient sconfig  -- evaluate to check for errors
      sess = emptySessionUI sconfig
  putSession sess {schanF, sbinding}

handleSelfAI :: ( MonadClientSetup m
                , MonadAtomic m )
             => UpdAtomic -> m ()
{-# INLINE handleSelfAI #-}
handleSelfAI cmdA = do
  cmds <- cmdAtomicFilterCli cmdA
  mapM_ (\ !c -> cmdAtomicSemCli c
                 >> execUpdAtomic c) cmds
  -- mapM_ (storeUndo . UpdAtomic) cmds

handleSelfUI :: ( MonadClientSetup m
                , MonadClientUI m
                , MonadAtomic m )
             => UpdAtomic -> m ()
{-# INLINE handleSelfUI #-}
handleSelfUI cmdA = do
  cmds <- cmdAtomicFilterCli cmdA
  let handle !c = do
        -- Avoid leaking the whole client state.
        !StateClient{sdiscoKind, sdiscoAspect} <- getClient
        cmdAtomicSemCli c
        execUpdAtomic c
        displayRespUpdAtomicUI False sdiscoKind sdiscoAspect c
  mapM_ handle cmds
  -- mapM_ (storeUndo . UpdAtomic) cmds  -- TODO: only store cmdA?
