-- | Abstract syntax of client commands.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.Response
  ( Response(..)
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State

-- | Abstract syntax of client commands for both AI and UI clients.
data Response =
    RespUpdAtomic State UpdAtomic
  | RespUpdAtomicNoState UpdAtomic
  | RespQueryAI ActorId
  | RespSfxAtomic SfxAtomic
  | RespQueryUI
  deriving Show
