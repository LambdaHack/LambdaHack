-- | Atomic game state transformations.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic
  ( -- * Re-exported from "Game.LambdaHack.Atomic.MonadAtomic"
    MonadAtomic(..), AtomicFail(..)
    -- * Re-exported from "Game.LambdaHack.Atomic.CmdAtomic"
  , CmdAtomic(..), UpdAtomic(..), SfxAtomic(..), SfxMsg(..)
    -- * Re-exported from "Game.LambdaHack.Atomic.PosAtomicRead"
  , PosAtomic(..), posUpdAtomic, posSfxAtomic, breakUpdAtomic
  , seenAtomicCli, seenAtomicSer, generalMoveItem
    -- * Re-exported from "Game.LambdaHack.Atomic.MonadStateWrite"
  , MonadStateWrite(..)
    -- * Re-exported from "Game.LambdaHack.Atomic.HandleAtomicWrite"
  , handleUpdAtomic
  ) where

import Prelude ()

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Atomic.HandleAtomicWrite
import Game.LambdaHack.Atomic.MonadAtomic
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Atomic.PosAtomicRead
