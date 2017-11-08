-- | Abstract syntax of client commands.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client.Response
  ( Response(..)
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.State

-- | Abstract syntax of client commands for both AI and UI clients.
data Response =
    RespUpdAtomic State UpdAtomic
  | RespUpdAtomicNoState UpdAtomic
  | RespQueryAI ActorId
  | RespSfxAtomic SfxAtomic
  | RespQueryUI
  deriving Show
