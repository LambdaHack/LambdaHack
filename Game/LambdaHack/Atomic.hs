-- | Atomic game state transformations.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic
  ( -- * Re-exported from "Game.LambdaHack.Atomic.MonadAtomic"
    MonadAtomic(..)
    -- * Re-exported from "Game.LambdaHack.Atomic.CmdAtomic"
  , CmdAtomic(..), UpdAtomic(..), SfxAtomic(..), HitAtomic(..)
    -- * Re-exported from "Game.LambdaHack.Atomic.PosAtomicRead"
  , PosAtomic(..), posUpdAtomic, posSfxAtomic, seenAtomicCli, generalMoveItem
  , posProjBody
  ) where

import Prelude ()

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Atomic.MonadAtomic
import Game.LambdaHack.Atomic.PosAtomicRead
