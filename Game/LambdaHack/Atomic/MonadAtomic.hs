-- | Atomic monads.
module Game.LambdaHack.Atomic.MonadAtomic
  ( MonadAtomic(..)
  , broadcastUpdAtomic,  broadcastSfxAtomic
  ) where

import Data.Key (mapWithKeyM_)

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.State

class MonadStateRead m => MonadAtomic m where
  execAtomic    :: CmdAtomic -> m ()
  execUpdAtomic :: UpdAtomic -> m ()
  execUpdAtomic = execAtomic . UpdAtomic
  execSfxAtomic :: SfxAtomic -> m ()
  execSfxAtomic = execAtomic . SfxAtomic

broadcastUpdAtomic :: MonadAtomic m
                   => (FactionId -> UpdAtomic) -> m ()
broadcastUpdAtomic fcmd = do
  factionD <- getsState sfactionD
  mapWithKeyM_ (\fid _ -> execUpdAtomic $ fcmd fid) factionD

broadcastSfxAtomic :: MonadAtomic m
                   => (FactionId -> SfxAtomic) -> m ()
broadcastSfxAtomic fcmd = do
  factionD <- getsState sfactionD
  mapWithKeyM_ (\fid _ -> execSfxAtomic $ fcmd fid) factionD
