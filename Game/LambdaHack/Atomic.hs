-- | Atomic game state transformations. TODO: haddocks.
--
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic
  ( -- * Re-exported from MonadAtomic
    MonadAtomic(..)
  , broadcastUpdAtomic, broadcastSfxAtomic
    -- * Re-exported from CmdAtomic
  , CmdAtomic(..), UpdAtomic(..), SfxAtomic(..), HitAtomic(..)
    -- * Re-exported from PosAtomicRead
  , PosAtomic(..), posUpdAtomic, posSfxAtomic, seenAtomicCli, generalMoveItem
  ) where

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Atomic.MonadAtomic
import Game.LambdaHack.Atomic.PosAtomicRead
