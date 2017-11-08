-- | Atomic game state transformations.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic
  ( -- * Re-exported from "Game.LambdaHack.Atomic.CmdAtomic"
    CmdAtomic(..), UpdAtomic(..), SfxAtomic(..), SfxMsg(..)
    -- * Re-exported from "Game.LambdaHack.Atomic.PosAtomicRead"
  , PosAtomic(..), posUpdAtomic, posSfxAtomic, breakUpdAtomic
  , seenAtomicCli, seenAtomicSer
    -- * Re-exported from "Game.LambdaHack.Atomic.MonadStateWrite"
  , MonadStateWrite(..), AtomicFail(..)
    -- * Re-exported from "Game.LambdaHack.Atomic.HandleAtomicWrite"
  , handleUpdAtomic
  ) where

import Prelude ()

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Atomic.HandleAtomicWrite
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Atomic.PosAtomicRead
