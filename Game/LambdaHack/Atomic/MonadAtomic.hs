-- | Atomic monads for handling atomic game state transformations.
module Game.LambdaHack.Atomic.MonadAtomic
  ( MonadAtomic(..)
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception

-- | The monad for executing atomic game state transformations.
class MonadStateRead m => MonadAtomic m where
  -- | Execute an atomic command that really changes the state.
  execUpdAtomic :: UpdAtomic -> m ()
  -- | Execute an atomic command that really changes the state.
  -- Catch 'AtomicFail' and indicate if it was in fact raised.
  execUpdAtomicCatch :: UpdAtomic -> m Bool
  -- | Execute an atomic command that only displays special effects.
  execSfxAtomic :: SfxAtomic -> m ()
  execSendPer :: FactionId -> LevelId
              -> Perception -> Perception -> Perception -> m ()
