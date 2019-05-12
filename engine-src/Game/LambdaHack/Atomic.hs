-- | Atomic game state transformations, their representation and semantics.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic
  ( -- * Re-exported from "Game.LambdaHack.Atomic.CmdAtomic"
    CmdAtomic(..), UpdAtomic(..), HearMsg(..), SfxAtomic(..), SfxMsg(..)
    -- * Re-exported from "Game.LambdaHack.Atomic.HandleAtomicWrite"
  , handleUpdAtomic
    -- * Re-exported from "Game.LambdaHack.Atomic.PosAtomicRead"
  , PosAtomic(..), posUpdAtomic, posSfxAtomic, iidUpdAtomic, iidSfxAtomic
  , breakUpdAtomic, seenAtomicCli, seenAtomicGeneralCli, seenAtomicSer
    -- * Re-exported from "Game.LambdaHack.Atomic.MonadStateWrite"
  , MonadStateWrite(..), AtomicFail(..)
  ) where

import Prelude ()

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Atomic.HandleAtomicWrite
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Atomic.PosAtomicRead
