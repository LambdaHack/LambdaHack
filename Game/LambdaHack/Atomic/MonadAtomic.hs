-- | Atomic monads for handling atomic game state transformations.
module Game.LambdaHack.Atomic.MonadAtomic
  ( MonadAtomic(..)
  , broadcastUpdAtomic,  broadcastSfxAtomic
  ) where

import Data.Key (mapWithKeyM_)

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.State

-- | The monad for executing atomic game state transformations.
class MonadStateRead m => MonadAtomic m where
  -- | Execute an arbitrary atomic game state transformation.
  execAtomic    :: CmdAtomic -> m ()
  -- | Execute an atomic command that really changes the state.
  execUpdAtomic :: UpdAtomic -> m ()
  execUpdAtomic = execAtomic . UpdAtomic
  -- | Execute an atomic command that only displays special effects.
  execSfxAtomic :: SfxAtomic -> m ()
  execSfxAtomic = execAtomic . SfxAtomic

-- | Create and broadcast a set of atomic updates, one for each client.
broadcastUpdAtomic :: MonadAtomic m
                   => (FactionId -> UpdAtomic) -> m ()
broadcastUpdAtomic fcmd = do
  factionD <- getsState sfactionD
  mapWithKeyM_ (\fid _ -> execUpdAtomic $ fcmd fid) factionD

-- | Create and broadcast a set of atomic special effects, one for each client.
broadcastSfxAtomic :: MonadAtomic m
                   => (FactionId -> SfxAtomic) -> m ()
broadcastSfxAtomic fcmd = do
  factionD <- getsState sfactionD
  mapWithKeyM_ (\fid _ -> execSfxAtomic $ fcmd fid) factionD
